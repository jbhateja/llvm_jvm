//===  JVMOffsetAllocator.cpp - JVM Offset allocation Pass -----===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
// This pass assigns offset to arguments and virtual registers. These offsets
// are indices to Local Variable Array created during runtime. 
//===----------------------------------------------------------------------===//

#include "JVMInstrInfo.h"
#include "JVMMachineFunctionInfo.h"
#include "JVMMCOpcodeUtils.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Analysis/AliasAnalysis.h"
#include "llvm/CodeGen/JVMCommonUtils.h"
#include "llvm/CodeGen/MachineBasicBlock.h"
#include "llvm/CodeGen/MachineDominators.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/MachineFunctionPass.h"
#include "llvm/CodeGen/MachineInstr.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/MachineOperand.h"
#include "llvm/CodeGen/MachineRegisterInfo.h"
#include "llvm/CodeGen/Passes.h"
#include "llvm/CodeGen/TargetInstrInfo.h"
#include "llvm/CodeGen/TargetOpcodes.h"
#include "llvm/CodeGen/TargetRegisterInfo.h"
#include "llvm/CodeGen/TargetSubtargetInfo.h"
#include "llvm/MC/MCInstrDesc.h"
#include "llvm/MC/MCRegisterInfo.h"
#include "llvm/Pass.h"
#include "llvm/Support/Allocator.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/RecyclingAllocator.h"
#include "llvm/Support/raw_ostream.h"
#include <cassert>
#include <iterator>
#include <utility>
#include <vector>

using namespace llvm;

#define DEBUG_TYPE "jvm-offset-alloc"

STATISTIC(NumLocals, "Number of allocated locals.");

namespace llvm {
static void initializeJVMOffsetAllocatorPass(PassRegistry &);
}

namespace {

class JVMOffsetAllocator : public MachineFunctionPass {
  const JVMInstrInfo *TII;
  const JVMRegisterInfo *TRI;
  MachineFunction *MF;
  MachineRegisterInfo *MRI;
  MachineDominatorTree *DT;

public:
  static char ID; // Pass identification

  JVMOffsetAllocator() : MachineFunctionPass(ID) {
    initializeJVMOffsetAllocatorPass(*PassRegistry::getPassRegistry());
  }

  bool runOnMachineFunction(MachineFunction &MF) override;

  void getAnalysisUsage(AnalysisUsage &AU) const override {
    AU.setPreservesCFG();
    MachineFunctionPass::getAnalysisUsage(AU);
    AU.addRequired<MachineDominatorTree>();
  }

  void releaseMemory() override {}

private:
  bool ProcessBlockAllocation(MachineBasicBlock *MBB);
  void PerformAllocation(MachineDomTreeNode *Node);

  void EnterScope(MachineBasicBlock *MBB);
  void ExitScope(MachineBasicBlock *MBB);
  void ExitScopeIfDone(MachineDomTreeNode *Node,
                       DenseMap<MachineDomTreeNode *, unsigned> &OpenChildren);

  void PerformAllocationForScratchSet();
  void PerformArgumentAllocation(MachineBasicBlock *MBB);
};
} // end anonymous namespace

char JVMOffsetAllocator::ID = 0;

INITIALIZE_PASS_BEGIN(JVMOffsetAllocator, DEBUG_TYPE,
                      "JVM Offset Allocation", false,
                      false)
INITIALIZE_PASS_DEPENDENCY(MachineDominatorTree)
INITIALIZE_PASS_END(JVMOffsetAllocator, DEBUG_TYPE,
                    "JVM Offset Allocation", false,
                    false)

void JVMOffsetAllocator::EnterScope(MachineBasicBlock *MBB) {
  DEBUG(dbgs() << "Entering: " << MBB->getName() << '\n');
}

void JVMOffsetAllocator::ExitScope(MachineBasicBlock *MBB) {
  DEBUG(dbgs() << "Exiting: " << MBB->getName() << '\n');
}

static bool isEntryBlock(MachineBasicBlock *MBB) {
  if (MBB->pred_empty()) {
    assert(MBB->getNumber() == 0 && "Entry block with non-zero block Number.");
    return true;
  } else
    return false;
}

static bool isAllocationCandidate(const JVMInstrInfo *TII, MachineInstr *MI) {
  if (MI->isReturn())
    return false;
  if (MI->skipAllocation())
    return false;
  return true;
}

static bool isArgument(MachineInstr *MI) {
  unsigned Opcode = MI->getOpcode();
  switch (Opcode) {
  default:
    return false;
  case JVM::ARGUMENT_I8:
  case JVM::ARGUMENT_I16:
  case JVM::ARGUMENT_I32:
  case JVM::ARGUMENT_I64:
  case JVM::ARGUMENT_F32:
  case JVM::ARGUMENT_F64:
    return true;
  }
}

void JVMOffsetAllocator::PerformArgumentAllocation(MachineBasicBlock *MBB) {
  SmallVector<MachineInstr *, 32> Arguments;
  JVMFunctionInfo *FuncInfo = MF->getInfo<JVMFunctionInfo>();
  const Function &Func = MBB->getParent()->getFunction();
  const Module * Mod = Func.getParent();

  bool IsInstMethod = Func.hasFnAttribute(Attribute::JVMInstMethod);
  if (IsInstMethod) {
    // Reserve 0th offset for constant global structure object.
    const GlobalVariable *GCStructObj = Mod->getNamedGlobal("ConstantInitStruct");
    assert(GCStructObj && "Global constants structure not present");
    FuncInfo->setLocalsLimit(1);    
    FuncInfo->setEmitGlobConsts();
  }

  auto SortArguments = [](MachineInstr *I1, MachineInstr *I2) {
    assert(I1->getOperand(1).isImm() && I2->getOperand(1).isImm() &&
           "Argument has non-numeric operand.");
    const MachineOperand &Imm1 = I1->getOperand(1);
    const MachineOperand &Imm2 = I2->getOperand(1);
    return Imm1.getImm() < Imm2.getImm();
  };

  for (MachineBasicBlock::iterator I = MBB->begin(), E = MBB->end(); I != E;
       I++) {
    MachineInstr *MI = &*I;
    if (isArgument(MI))
      Arguments.push_back(MI);
  }

  std::sort(Arguments.begin(), Arguments.end(), SortArguments);

  for (auto *Arg : Arguments) {
    MachineOperand &MO = Arg->getOperand(0);
    FuncInfo->setRegisterOffset(MO.getReg());

    DEBUG(dbgs() << "Examining: " << *Arg);
    DEBUG(dbgs() << "Allocating offset to Argument : Reg = " << MO.getReg()
                 << " Offset = " << FuncInfo->getRegisterOffset(MO.getReg())
                 << "\n");
    NumLocals++;
  }
}

bool JVMOffsetAllocator::ProcessBlockAllocation(MachineBasicBlock *MBB) {
  JVMFunctionInfo *FuncInfo = MF->getInfo<JVMFunctionInfo>();

  // For entry block perform allocation of arguments followed by
  // allocation of virtual registers contained in ScratchAllocationSet.
  if (isEntryBlock(MBB)) {
    PerformArgumentAllocation(MBB);
    PerformAllocationForScratchSet();
  }

  for (MachineBasicBlock::iterator I = MBB->begin(), E = MBB->end(); I != E;) {
    MachineInstr *MI = &*I;
    ++I;

    if (!isAllocationCandidate(TII, MI) || isArgument(MI))
      continue;

    unsigned NumDefs = MI->getDesc().getNumDefs();
    for (unsigned i = 0; i < NumDefs; ++i) {
      MachineOperand &MO = MI->getOperand(i);
      if (!MO.isReg() || MO.isImplicit())
        continue;

      assert(TargetRegisterInfo::isVirtualRegister(MO.getReg()) &&
             "Do not perform allocation for physical register defs!");

      if (MI->isMoveImmediate()) {
        // For immediate moves definition offsets are set to a -ve value (-1).
        FuncInfo->setRegisterOffset(MO.getReg(), -1);
      } else if (MI->mayLoad() || MI->getDesc().isRegCConv()) {
        // In case of loads offset are propagated from loads to their
        // definitions.
        unsigned UseOffset;
        MachineOperand LoadSrc = MI->getOperand(1);
        if (LoadSrc.isReg())
          UseOffset = FuncInfo->getRegisterOffset(MI->getOperand(1).getReg());
        else if (LoadSrc.isGlobal()) {
          const GlobalValue * GV = LoadSrc.getGlobal();
          assert(GV->getName() == "ConstantInitStruct"); 
          UseOffset = 0;
        }
        FuncInfo->setRegisterOffset(MO.getReg(), UseOffset);
      } else if (MI->mayStore()) {
        // Copy elision pass breaks the SSA nature and hence introduce
        // multiple stores to same virtual register.
        if (!FuncInfo->isRegisterOffsetAllocated(MO.getReg()))
          FuncInfo->setRegisterOffset(MO.getReg());
      } else
        FuncInfo->setRegisterOffset(MO.getReg());

      DEBUG(dbgs() << "Examining: " << *MI);
      DEBUG(dbgs() << "Allocating : Reg = " << MO.getReg() << " Offset = "
                   << FuncInfo->getRegisterOffset(MO.getReg()) << "\n");
      NumLocals++;
    }
  }
  return false;
}

/// ExitScopeIfDone - Destroy scope for the MBB that corresponds to the given
/// dominator tree node if its a leaf or all of its children are done. Walk
/// up the dominator tree to destroy ancestors which are now done.
void JVMOffsetAllocator::ExitScopeIfDone(
    MachineDomTreeNode *Node,
    DenseMap<MachineDomTreeNode *, unsigned> &OpenChildren) {
  if (OpenChildren[Node])
    return;

  // Pop scope.
  ExitScope(Node->getBlock());

  // Now traverse upwards to pop ancestors whose offsprings are all done.
  while (MachineDomTreeNode *Parent = Node->getIDom()) {
    unsigned Left = --OpenChildren[Parent];
    if (Left != 0)
      break;
    ExitScope(Parent->getBlock());
    Node = Parent;
  }
}

void JVMOffsetAllocator::PerformAllocationForScratchSet() {
  JVMFunctionInfo *FuncInfo = MF->getInfo<JVMFunctionInfo>();

  // Iterate over elements in scratchAllocations and assign them offsets.
  JVMFunctionInfo::ScratchAllocationSetIter Itr =
      FuncInfo->getScratchAllocationSet().begin();
  while (Itr != FuncInfo->getScratchAllocationSet().end()) {
    FuncInfo->setRegisterOffset(*Itr);
    DEBUG(dbgs() << "Allocating from StratchAllocationSet : Reg = " << *Itr
                 << " Offset = " << FuncInfo->getRegisterOffset(*Itr) << "\n");
    Itr++;
    NumLocals++;
  }
  FuncInfo->clearScratchAllocationSet();
}

void JVMOffsetAllocator::PerformAllocation(MachineDomTreeNode *Node) {
  SmallVector<MachineDomTreeNode *, 32> Scopes;
  SmallVector<MachineDomTreeNode *, 8> WorkList;
  DenseMap<MachineDomTreeNode *, unsigned> OpenChildren;
  JVMFunctionInfo *FuncInfo = MF->getInfo<JVMFunctionInfo>();

  // Initialize the Local variable array counter.
  FuncInfo->setLocalsLimit(0);

  // Perform a DFS walk to determine the order of visit.
  WorkList.push_back(Node);
  do {
    Node = WorkList.pop_back_val();
    Scopes.push_back(Node);
    const std::vector<MachineDomTreeNode *> &Children = Node->getChildren();
    OpenChildren[Node] = Children.size();
    for (MachineDomTreeNode *Child : Children)
      WorkList.push_back(Child);
  } while (!WorkList.empty());

  for (MachineDomTreeNode *Node : Scopes) {
    MachineBasicBlock *MBB = Node->getBlock();
    EnterScope(MBB);
    ProcessBlockAllocation(MBB);
    // If it's a leaf node, it's done. Traverse upwards to pop ancestors.
    ExitScopeIfDone(Node, OpenChildren);
  }
}

bool JVMOffsetAllocator::runOnMachineFunction(MachineFunction &MFN) {
  MF = &MFN;
  TII = static_cast<const JVMInstrInfo *>(MF->getSubtarget().getInstrInfo());
  TRI = static_cast<const JVMRegisterInfo *>(
      MF->getSubtarget().getRegisterInfo());
  MRI = &MF->getRegInfo();
  DT = &getAnalysis<MachineDominatorTree>();

  DEBUG(dbgs() << "\n******** JVM Offset allocation ********\n");
  PerformAllocation(DT->getRootNode());
  return true;
}

namespace llvm {
FunctionPass *createJVMOffsetAllocator() { return new JVMOffsetAllocator(); }
} // namespace llvm
