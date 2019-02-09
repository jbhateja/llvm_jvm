//===  JVMLoadStoreEliminationOpt.cpp - JVM Load/Store Elimination OptPass-----===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===--------------------------------------------------------------------------===//
// This pass performs redundant load/store elimination.
// It looks at all the MIs which are post-allocation store insertion
// candidates, it then checks if next instruction is a load operation on the
// recently computed value, if there is one then it marks no-allocation
// flags over current MI and eliminates the redundant load.
// No-allocation flag will prevent offset allocation to result of MI
// since it is directly consumable by the succeeding operation.
//===--------------------------------------------------------------------------===//

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

#define DEBUG_TYPE "jvm-load-store-elimination-opt"

STATISTIC(NumEliminations, "Number of Eliminations.");

namespace llvm {
static void initializeJVMLoadStoreEliminationOptPass(PassRegistry &);
bool isStoreCandidate(const MachineInstr &MI);
} // namespace llvm

namespace {

class JVMLoadStoreEliminationOpt : public MachineFunctionPass {
  const JVMInstrInfo *TII;
  const JVMRegisterInfo *TRI;
  MachineFunction *MF;
  MachineRegisterInfo *MRI;
  MachineDominatorTree *DT;

public:
  static char ID; // Pass identification

  JVMLoadStoreEliminationOpt() : MachineFunctionPass(ID) {
    initializeJVMLoadStoreEliminationOptPass(*PassRegistry::getPassRegistry());
  }

  bool runOnMachineFunction(MachineFunction &MF) override;

  void getAnalysisUsage(AnalysisUsage &AU) const override {
    AU.setPreservesCFG();
    MachineFunctionPass::getAnalysisUsage(AU);
    AU.addRequired<MachineDominatorTree>();
  }

  void releaseMemory() override {}

private:
  bool ProcessBlock(MachineBasicBlock *MBB);
  void PerformOptimization(MachineDomTreeNode *Node);

  void EnterScope(MachineBasicBlock *MBB);
  void ExitScope(MachineBasicBlock *MBB);
  void ExitScopeIfDone(MachineDomTreeNode *Node,
                       DenseMap<MachineDomTreeNode *, unsigned> &OpenChildren);
};
} // end anonymous namespace

char JVMLoadStoreEliminationOpt::ID = 0;

INITIALIZE_PASS_BEGIN(JVMLoadStoreEliminationOpt, DEBUG_TYPE,
                      "JVM Load/Store Elimination Optimization", false, false)
INITIALIZE_PASS_DEPENDENCY(MachineDominatorTree)
INITIALIZE_PASS_END(JVMLoadStoreEliminationOpt, DEBUG_TYPE,
                    "JVM Load/Store Elimination Optimization", false, false)

void JVMLoadStoreEliminationOpt::EnterScope(MachineBasicBlock *MBB) {
  LLVM_DEBUG(dbgs() << "Entering: " << MBB->getName() << '\n');
}

void JVMLoadStoreEliminationOpt::ExitScope(MachineBasicBlock *MBB) {
  LLVM_DEBUG(dbgs() << "Exiting: " << MBB->getName() << '\n');
}

bool JVMLoadStoreEliminationOpt::ProcessBlock(MachineBasicBlock *MBB) {
  SmallVector<MachineInstr *, 4> RemoveInstr;

  for (MachineBasicBlock::iterator I = MBB->begin(), E = MBB->end(); I != E;) {
    MachineInstr *MI = &*I;
    ++I;

    if (!llvm::isStoreCandidate(*MI))
      continue;

    unsigned NumDefs = MI->getDesc().getNumDefs();
    if (1 != NumDefs)
      continue;

    MachineOperand &MO = MI->getOperand(0);
    if (!MO.isReg() || MO.isImplicit())
      continue;

    if (!MRI->hasOneUse(MO.getReg()))
      continue;

    MachineInstr &UseMI = *MRI->use_instr_begin(MO.getReg());
    if (!JVMMCOpcodeUtils::IsLoadInstr(UseMI.getOpcode()) ||
        &UseMI != MI->getNextNode())
      continue;

    assert(MRI->hasOneUse(UseMI.getOperand(0).getReg()));
    MachineOperand &LoadDstUse = *MRI->use_begin(UseMI.getOperand(0).getReg());
    LoadDstUse.substVirtReg(MI->getOperand(0).getReg(), 0, *TRI);

    MI->setFlag(MachineInstr::NoAllocation);
    RemoveInstr.push_back(&UseMI);
    NumEliminations += 2;
  }

  for (auto MI : RemoveInstr)
    MI->eraseFromParent();

  return false;
}

/// ExitScopeIfDone - Destroy scope for the MBB that corresponds to the given
/// dominator tree node if its a leaf or all of its children are done. Walk
/// up the dominator tree to destroy ancestors which are now done.
void JVMLoadStoreEliminationOpt::ExitScopeIfDone(
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

void JVMLoadStoreEliminationOpt::PerformOptimization(MachineDomTreeNode *Node) {
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
    ProcessBlock(MBB);
    // If it's a leaf node, it's done. Traverse upwards to pop ancestors.
    ExitScopeIfDone(Node, OpenChildren);
  }
}

bool JVMLoadStoreEliminationOpt::runOnMachineFunction(MachineFunction &MFN) {
  MF = &MFN;
  TII = static_cast<const JVMInstrInfo *>(MF->getSubtarget().getInstrInfo());
  TRI = static_cast<const JVMRegisterInfo *>(
      MF->getSubtarget().getRegisterInfo());
  MRI = &MF->getRegInfo();
  DT = &getAnalysis<MachineDominatorTree>();

  LLVM_DEBUG(dbgs()
        << "\n******** JVM Load/Store Elimination Optimization ********\n");
  PerformOptimization(DT->getRootNode());
  return true;
}

namespace llvm {
FunctionPass *createJVMLoadStoreEliminationOpt() {
  return new JVMLoadStoreEliminationOpt();
}
} // namespace llvm
