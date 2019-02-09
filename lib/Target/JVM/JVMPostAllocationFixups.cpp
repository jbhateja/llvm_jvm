//===------  JVMPostAllocationFixups.cpp - JVM Offset allocation Pass -----===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
// This pass performs post offset allocation fixups over machine instructions.
//===----------------------------------------------------------------------===//

#include "JVMInstrInfo.h"
#include "JVMMachineFunctionInfo.h"
#include "JVMMCOpcodeUtils.h"
#include "JVMMICommonUtils.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Analysis/AliasAnalysis.h"
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

#define DEBUG_TYPE "jvm-post-allocation-fixups"

STATISTIC(NumFixups, "Number of Fixups.");

namespace llvm {
static void initializeJVMPostAllocationFixupsPass(PassRegistry &);
bool isStoreCandidate(const MachineInstr &MI) {
  if (MI.skipAllocation())
    return false;
  if (MI.mayStore() || MI.mayLoad() || MI.isReturn() || MI.isCompare() ||
      MI.hasUnmodeledSideEffects() || MI.isRematerializable())
    return false;
  else
    return true;
}
}

namespace {

class JVMPostAllocationFixups : public MachineFunctionPass {
  const JVMInstrInfo *TII;
  const JVMRegisterInfo *TRI;
  MachineFunction *MF;
  MachineRegisterInfo *MRI;
  MachineDominatorTree *DT;

public:
  static char ID; // Pass identification

  JVMPostAllocationFixups() : MachineFunctionPass(ID) {
    initializeJVMPostAllocationFixupsPass(*PassRegistry::getPassRegistry());
  }

  bool runOnMachineFunction(MachineFunction &MF) override;

  void getAnalysisUsage(AnalysisUsage &AU) const override {
    AU.setPreservesCFG();
    MachineFunctionPass::getAnalysisUsage(AU);
    AU.addRequired<MachineDominatorTree>();
  }

  void releaseMemory() override {}

private:
  bool ProcessBlockInsertion(MachineBasicBlock *MBB);
  void PerformStoreInsertion(MachineDomTreeNode *Node);

  bool ProcessReturnFixup(MachineBasicBlock *MBB);
  void PerformReturnFixup(MachineDomTreeNode *Node);

  void EnterScope(MachineBasicBlock *MBB);
  void ExitScope(MachineBasicBlock *MBB);
  void ExitScopeIfDone(MachineDomTreeNode *Node,
                       DenseMap<MachineDomTreeNode *, unsigned> &OpenChildren);
};
} // end anonymous namespace

char JVMPostAllocationFixups::ID = 0;

INITIALIZE_PASS_BEGIN(JVMPostAllocationFixups, DEBUG_TYPE,
                      "JVM Post Allocation Fixups", false, false)
INITIALIZE_PASS_DEPENDENCY(MachineDominatorTree)
INITIALIZE_PASS_END(JVMPostAllocationFixups, DEBUG_TYPE,
                    "JVM Post Allocation Fixups", false, false)

void JVMPostAllocationFixups::EnterScope(MachineBasicBlock *MBB) {
  LLVM_DEBUG(dbgs() << "Entering: " << MBB->getName() << '\n');
}

void JVMPostAllocationFixups::ExitScope(MachineBasicBlock *MBB) {
  LLVM_DEBUG(dbgs() << "Exiting: " << MBB->getName() << '\n');
}

/// ExitScopeIfDone - Destroy scope for the MBB that corresponds to the given
/// dominator tree node if its a leaf or all of its children are done. Walk
/// up the dominator tree to destroy ancestors which are now done.
void JVMPostAllocationFixups::ExitScopeIfDone(
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

static bool isBinaryInstruction(const MachineInstr &MI) {
  if (MI.getNumOperands() == 3 && MI.getDesc().getNumDefs() == 1)
    return true;
  else
    return false;
}


bool JVMPostAllocationFixups::ProcessBlockInsertion(MachineBasicBlock *MBB) {
  bool Change = false;

  MachineInstr *NewMI = nullptr;
  JVMFunctionInfo *FuncInfo = MF->getInfo<JVMFunctionInfo>();
  MachineRegisterInfo &MRI = MF->getRegInfo();

  for (MachineBasicBlock::iterator I = MBB->begin(), E = MBB->end(); I != E;) {
    MachineInstr *MI = &*I;
    ++I;

    if (!isStoreCandidate(*MI))
      continue;

    LLVM_DEBUG(dbgs() << "Examining: " << *MI);
    unsigned NumDefs = MI->getDesc().getNumDefs();

    auto IdenticalOperands = [&](MachineInstr *MI,
                                 MachineInstr **DelMI) -> bool {
      MachineOperand Opr1 = MI->getOperand(1);
      MachineOperand Opr2 = MI->getOperand(2);
      if (Opr1.isReg() && Opr2.isReg() &&
          MRI.getVRegDef(Opr1.getReg())
              ->isIdenticalTo(*MRI.getVRegDef(Opr2.getReg()),
                              MachineInstr::IgnoreDefs)) {
        *DelMI = MRI.getVRegDef(Opr2.getReg());
        return true;
      }
      return false;
    };

    MachineInstr *DelMI = nullptr;
    // Insert a DUP instruction for a binary instruction having same operands.
    if (isBinaryInstruction(*MI) && IdenticalOperands(MI, &DelMI)) {
      const MCInstrDesc &Desc = DelMI->getDesc();
      assert(Desc.getNumDefs() == 1);
      assert(MRI.hasOneUse(DelMI->getOperand(0).getReg()));

      LLVM_DEBUG(dbgs() << "Same binary operands, inserting a DUP instruction.\n");
      unsigned DelMIDefReg = DelMI->getOperand(0).getReg();
      NewMI = BuildMI(*MBB, MI, MI->getDebugLoc(), TII->get(JVM::DUP))
              .addDef(DelMIDefReg);

      DelMI->eraseFromParent();
    }

    // Insert a STORE for each def register of an instruction.
    for (unsigned i = 0; i < NumDefs; ++i) {
      MachineOperand &MO = MI->getOperand(i);
      if (!MO.isReg() || MO.isImplicit())
        continue;

      assert(TargetRegisterInfo::isVirtualRegister(MO.getReg()));

      unsigned StoreOpcode =
          JVMMCOpcodeUtils::GetStoreOpcode(MRI.getRegClass(MO.getReg()));
      LLVM_DEBUG(dbgs() << "Inserting Store instruction.\n");
      NewMI = BuildMI(*MBB, I, MI->getDebugLoc(), TII->get(StoreOpcode))
                  .addImm(FuncInfo->getRegisterOffset(MO.getReg()));
      NumFixups++;
      if (NewMI != nullptr)
        Change = true;
    }
  }
  return Change;
}

void JVMPostAllocationFixups::PerformStoreInsertion(MachineDomTreeNode *Node) {
  SmallVector<MachineDomTreeNode *, 32> Scopes;
  SmallVector<MachineDomTreeNode *, 8> WorkList;
  DenseMap<MachineDomTreeNode *, unsigned> OpenChildren;

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
    ProcessBlockInsertion(MBB);
    // If it's a leaf node, it's done. Traverse upwards to pop ancestors.
    ExitScopeIfDone(Node, OpenChildren);
  }
}

static bool isReturnFixupCandidate(MachineRegisterInfo &MRI,
                                   const MachineInstr &MI) {
  if (MI.isReturn() && MI.getNumOperands()) {
    MachineInstr *RetOpDef = MRI.getVRegDef(MI.getOperand(0).getReg());
    if (!RetOpDef->mayLoad())
      return true;
  }
  return false;
}

bool JVMPostAllocationFixups::ProcessReturnFixup(MachineBasicBlock *MBB) {
  bool Change = false;
  MachineInstr *NewMI = nullptr;
  JVMFunctionInfo *FuncInfo = MF->getInfo<JVMFunctionInfo>();
  MachineRegisterInfo &MRI = MF->getRegInfo();

  for (MachineBasicBlock::iterator I = MBB->begin(), E = MBB->end(); I != E;) {
    bool isInt;
    MachineInstr *ConstDriver = nullptr;
    MachineInstr *MI = &*I;
    ++I;

    if (!isReturnFixupCandidate(MRI, *MI))
      continue;

    LLVM_DEBUG(dbgs() << "Examining: " << *MI);
    MachineOperand &RetVal = MI->getOperand(0);
    assert(RetVal.isReg() && "Register operand expected");

    unsigned RetReg = RetVal.getReg();
    const TargetRegisterClass *RC = MRI.getRegClass(RetReg);
    unsigned NewReg = MRI.createVirtualRegister(RC);

    if (HasConstantDriver(&MRI, RetVal, isInt, &ConstDriver)) {
      unsigned LDCOpcode = isInt ? JVM::CSTLDCI32 : JVM::LDCF32;
      NewMI = BuildMI(*MBB, MI, MI->getDebugLoc(), TII->get(LDCOpcode))
                  .addDef(NewReg)
                  .addImm(ConstDriver->getOperand(1).getImm());
      ConstDriver->eraseFromParent();
    } else {
      unsigned LoadOpcode = JVMMCOpcodeUtils::GetLoadOpcode(RC);
      NewMI = BuildMI(*MBB, MI, MI->getDebugLoc(), TII->get(LoadOpcode))
                  .addDef(NewReg)
                  .addImm(FuncInfo->getRegisterOffset(RetReg));
    }
    FuncInfo->setRegisterOffset(NewReg,
                                FuncInfo->getRegisterOffset(RetVal.getReg()));

    MachineOperand NewRetVal = MachineOperand::CreateReg(NewReg, false);
    MI->RemoveOperand(0);
    MI->addOperand(*MF, NewRetVal);
    MRI.addRegOperandToUseList(&NewRetVal);

    NumFixups++;
    if (NewMI != nullptr)
      Change = true;
  }
  return Change;
}

void JVMPostAllocationFixups::PerformReturnFixup(MachineDomTreeNode *Node) {
  SmallVector<MachineDomTreeNode *, 32> Scopes;
  SmallVector<MachineDomTreeNode *, 8> WorkList;
  DenseMap<MachineDomTreeNode *, unsigned> OpenChildren;

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
    ProcessReturnFixup(MBB);
    // If it's a leaf node, it's done. Traverse upwards to pop ancestors.
    ExitScopeIfDone(Node, OpenChildren);
  }
}

bool JVMPostAllocationFixups::runOnMachineFunction(MachineFunction &MFN) {
  MF = &MFN;
  TII = static_cast<const JVMInstrInfo *>(MF->getSubtarget().getInstrInfo());
  TRI = static_cast<const JVMRegisterInfo *>(
      MF->getSubtarget().getRegisterInfo());
  MRI = &MF->getRegInfo();
  DT = &getAnalysis<MachineDominatorTree>();

  LLVM_DEBUG(dbgs() << "\n******** JVM Store/Dup insertion ********\n");
  PerformStoreInsertion(DT->getRootNode());
  LLVM_DEBUG(dbgs() << "\n******** JVM Return Fixup ********\n");
  PerformReturnFixup(DT->getRootNode());
  return true;
}

namespace llvm {
FunctionPass *createJVMPostAllocationFixups() {
  return new JVMPostAllocationFixups();
}
} // namespace llvm
