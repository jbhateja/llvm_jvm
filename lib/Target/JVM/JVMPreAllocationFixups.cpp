//===  JVMPreAllocationFixups.cpp - JVM Copy elision Pass -----===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
// This pass performs pre offset allocation fixups over machine instructions.
//===----------------------------------------------------------------------===//

#include "JVMInstrInfo.h"
#include "JVMMachineFunctionInfo.h"
#include "JVMMCOpcodeUtils.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/DenseSet.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/Statistic.h"
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

#define DEBUG_TYPE "jvm-pre-allocation-fixups"

STATISTIC(NumFixupsInstrs, "Number of Fixups instructions.");

namespace llvm {
static void initializeJVMPreAllocationFixupsPass(PassRegistry &);
}

namespace {

class JVMPreAllocationFixups : public MachineFunctionPass {
public:
  using DeferredEraseSet = DenseSet<MachineInstr *>;
  using DeferredEraseSetIter = DenseSet<MachineInstr *>::iterator;

  static char ID; // Pass identification

  JVMPreAllocationFixups() : MachineFunctionPass(ID) {
    initializeJVMPreAllocationFixupsPass(*PassRegistry::getPassRegistry());
  }

  bool runOnMachineFunction(MachineFunction &MF) override;

  void getAnalysisUsage(AnalysisUsage &AU) const override {
    AU.setPreservesCFG();
    MachineFunctionPass::getAnalysisUsage(AU);
    AU.addRequired<MachineDominatorTree>();
  }

  void releaseMemory() override {}

private:
  void PerformExtendFixups(MachineDomTreeNode *Node);
  void ProcessExtendFixupsBlock(MachineBasicBlock *MBB);

  void EnterScope(MachineBasicBlock *MBB);
  void ExitScope(MachineBasicBlock *MBB);
  void ExitScopeIfDone(MachineDomTreeNode *Node,
                       DenseMap<MachineDomTreeNode *, unsigned> &OpenChildren);

  void AddToDeferredEraseSet(MachineInstr *MI);
  bool IsPreAllocationFixupsCandidate(MachineBasicBlock *MBB, MachineInstr *MI);
  void Cleanup();


  const JVMInstrInfo *TII;
  const JVMRegisterInfo *TRI;
  MachineFunction *MF;
  MachineRegisterInfo *MRI;
  MachineDominatorTree *DT;
  DeferredEraseSet EraseSet;
};

} // end anonymous namespace

char JVMPreAllocationFixups::ID = 0;

INITIALIZE_PASS_BEGIN(JVMPreAllocationFixups, DEBUG_TYPE,
                      "JVM Pre-allocation Fixups", false, false)
INITIALIZE_PASS_DEPENDENCY(MachineDominatorTree)
INITIALIZE_PASS_END(JVMPreAllocationFixups, DEBUG_TYPE, "JVM Pre-allocation Fixups",
                    false, false)

void JVMPreAllocationFixups::EnterScope(MachineBasicBlock *MBB) {
  DEBUG(dbgs() << "Entering: " << MBB->getName() << '\n');
}

void JVMPreAllocationFixups::ExitScope(MachineBasicBlock *MBB) {
  DEBUG(dbgs() << "Exiting: " << MBB->getName() << '\n');
}


void JVMPreAllocationFixups::Cleanup() {
  for (DeferredEraseSetIter I = EraseSet.begin(), E = EraseSet.end(); I != E;
       I++) {
    (*I)->eraseFromParent();
  }
  EraseSet.clear();
}


void JVMPreAllocationFixups::AddToDeferredEraseSet(MachineInstr *MI) {
  if (EraseSet.find(MI) == EraseSet.end())
    EraseSet.insert(MI);
}

bool IsExtendFixupCandidate(MachineRegisterInfo *MRI, MachineInstr *MI) {
  unsigned Opcode = MI->getOpcode();
  switch(Opcode) {
    default:
      return false;
    case JVM::EXTENDI82I64:
    case JVM::EXTENDI162I64:
    case JVM::EXTENDI82I32:
    case JVM::EXTENDI162I32: {
      MachineOperand RHS = MI->getOperand(1);
      MachineInstr *RHSdef = MRI->getVRegDef(RHS.getReg());
      return RHSdef->mayLoad();
    }
  }
}

void JVMPreAllocationFixups::ProcessExtendFixupsBlock(MachineBasicBlock *MBB) {
  for (MachineBasicBlock::iterator I = MBB->begin(), E = MBB->end(); I != E;) {
    MachineInstr *MI = &*I;
    ++I;

    if (!IsExtendFixupCandidate(MRI, MI))
      continue;

    NumFixupsInstrs++;
    DEBUG(dbgs() << "Found Extend, adding a STORE for its operand.\n"
                 << MBB->getName() << '\n');

    // Insert a STORE for RHS of a EXTEND instruction.
    MachineOperand MORHS = MI->getOperand(1);
    MachineOperand MOLHS = MI->getOperand(0);
    unsigned NewReg =
        MRI->createVirtualRegister(MRI->getRegClass(MOLHS.getReg()));

    unsigned StoreOpcode =
        JVMMCOpcodeUtils::GetStoreOpcode(MRI->getRegClass(MOLHS.getReg()));
    BuildMI(*MBB, MI, MI->getDebugLoc(), TII->get(StoreOpcode))
        .addDef(NewReg)
        .addUse(MORHS.getReg());

    MachineOperand RetVal = MachineOperand::CreateReg(NewReg, false);
    MI->RemoveOperand(1);
    MI->addOperand(*MF, RetVal);
    MRI->addRegOperandToUseList(&RetVal);
  }
}

/// ExitScopeIfDone - Destroy scope for the MBB that corresponds to the given
/// dominator tree node if its a leaf or all of its children are done. Walk
/// up the dominator tree to destroy ancestors which are now done.
void JVMPreAllocationFixups::ExitScopeIfDone(
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

void JVMPreAllocationFixups::PerformExtendFixups(MachineDomTreeNode *Node) {
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
    ProcessExtendFixupsBlock(MBB);
    // If it's a leaf node, it's done. Traverse upwards to pop ancestors.
    ExitScopeIfDone(Node, OpenChildren);
  }
  Cleanup();
}

bool JVMPreAllocationFixups::runOnMachineFunction(MachineFunction &MFN) {
  MF = &MFN;
  TII = static_cast<const JVMInstrInfo *>(MF->getSubtarget().getInstrInfo());
  TRI = static_cast<const JVMRegisterInfo *>(
      MF->getSubtarget().getRegisterInfo());
  MRI = &MF->getRegInfo();
  DT = &getAnalysis<MachineDominatorTree>();

  // Extend (widening casting operations) are fixed here ie. adding a 
  // STORE for its operand, this is being done here because during DAG
  // lowering we do not get chain node for binary/unary operations,
  // LOADs and STOREs are chained to avoid their reordering by scheduler.
  DEBUG(dbgs() << "\n******** JVM Extend Fixups ********\n");
  PerformExtendFixups(DT->getRootNode());
  return true;
}

namespace llvm {
FunctionPass *createJVMPreAllocationFixups() { return new JVMPreAllocationFixups(); }
} // namespace llvm
