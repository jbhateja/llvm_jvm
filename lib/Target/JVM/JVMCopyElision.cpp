//===  JVMCopyElision.cpp - JVM Copy elision Pass -------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
// This pass replaces COPY instructions by a sequence of LOAD and
// STORE instruction.
//===----------------------------------------------------------------------===//

#include "JVMInstrInfo.h"
#include "JVMMachineFunctionInfo.h"
#include "JVMMCOpcodeUtils.h"
#include "JVMMICommonUtils.h"
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

#define DEBUG_TYPE "jvm-copy-elision"

STATISTIC(NumCopyInstrs, "Number of COPY instructions.");

namespace llvm {
static void initializeJVMCopyElisionPass(PassRegistry &);
}

namespace {

class JVMCopyElision : public MachineFunctionPass {
public:
  using DeferredEraseSet = DenseSet<MachineInstr *>;
  using DeferredEraseSetIter = DenseSet<MachineInstr *>::iterator;

  static char ID; // Pass identification

  JVMCopyElision() : MachineFunctionPass(ID) {
    initializeJVMCopyElisionPass(*PassRegistry::getPassRegistry());
  }

  bool runOnMachineFunction(MachineFunction &MF) override;

  void getAnalysisUsage(AnalysisUsage &AU) const override {
    AU.setPreservesCFG();
    MachineFunctionPass::getAnalysisUsage(AU);
    AU.addRequired<MachineDominatorTree>();
  }

  void releaseMemory() override {}

private:
  void PerformCopyElision(MachineDomTreeNode *Node);
  void ProcessCopyElisionBlock(MachineBasicBlock *MBB);

  void EnterScope(MachineBasicBlock *MBB);
  void ExitScope(MachineBasicBlock *MBB);
  void ExitScopeIfDone(MachineDomTreeNode *Node,
                       DenseMap<MachineDomTreeNode *, unsigned> &OpenChildren);

  void AddToDeferredEraseSet(MachineInstr *MI);
  bool IsCopyElisionCandidate(MachineBasicBlock *MBB, MachineInstr *MI);
  void Cleanup();


  const JVMInstrInfo *TII;
  const JVMRegisterInfo *TRI;
  MachineFunction *MF;
  MachineRegisterInfo *MRI;
  MachineDominatorTree *DT;
  DeferredEraseSet EraseSet;
};
} // end anonymous namespace

char JVMCopyElision::ID = 0;

INITIALIZE_PASS_BEGIN(JVMCopyElision, DEBUG_TYPE,
                      "JVM Copy Elision", false, false)
INITIALIZE_PASS_DEPENDENCY(MachineDominatorTree)
INITIALIZE_PASS_END(JVMCopyElision, DEBUG_TYPE, "JVM Copy Elision",
                    false, false)

void JVMCopyElision::EnterScope(MachineBasicBlock *MBB) {
  DEBUG(dbgs() << "Entering: " << MBB->getName() << '\n');
}

void JVMCopyElision::ExitScope(MachineBasicBlock *MBB) {
  DEBUG(dbgs() << "Exiting: " << MBB->getName() << '\n');
}

void JVMCopyElision::Cleanup() {
  for (DeferredEraseSetIter I = EraseSet.begin(), E = EraseSet.end(); I != E;
       I++) {
    (*I)->eraseFromParent();
  }
  EraseSet.clear();
}

void JVMCopyElision::AddToDeferredEraseSet(MachineInstr *MI) {
  if (EraseSet.find(MI) == EraseSet.end())
    EraseSet.insert(MI);
}

bool JVMCopyElision::IsCopyElisionCandidate(MachineBasicBlock *MBB, MachineInstr *MI) {
  if(!MI->isCopyLike())
    return false;

  MachineOperand LHS = MI->getOperand(0);
  MachineOperand RHS = MI->getOperand(1);

  const TargetRegisterClass * LHSRC = MRI->getRegClass(LHS.getReg());
  const TargetRegisterClass * RHSRC = MRI->getRegClass(RHS.getReg());
  
  // Create a RC converter instruction in place of COPY.
  if (LHSRC != RHSRC) {
    DEBUG(dbgs() << "Found COPY with different RegClasses, replacing with RegClassConv.\n"
                 << MBB->getName() << '\n');
    BuildMI(*MBB, MI, MI->getDebugLoc(),
        TII->get(JVMMCOpcodeUtils::GetRCConvOpcode(LHSRC, RHSRC)))
         .addDef(LHS.getReg())
         .addUse(RHS.getReg());
    AddToDeferredEraseSet(MI);
    return false;
  }

  return true; 
}

void JVMCopyElision::ProcessCopyElisionBlock(MachineBasicBlock *MBB) {
  for (MachineBasicBlock::iterator I = MBB->begin(), E = MBB->end(); I != E;) {
    MachineInstr *MI = &*I;
    ++I;

    if (!IsCopyElisionCandidate(MBB, MI))
      continue;

    NumCopyInstrs++;
    DEBUG(dbgs() << "Found COPY, lowering to LOAD and STORE.\n"
                 << MBB->getName() << '\n');

    // Insert a LOAD for RHS of a COPY instruction.
    bool isInt;
    MachineInstr *ConstNode = nullptr;
    MachineOperand MORHS = MI->getOperand(1);
    unsigned NewReg =
        MRI->createVirtualRegister(MRI->getRegClass(MORHS.getReg()));
    if (HasConstantDriver(MRI, MORHS, isInt, &ConstNode)) {
      BuildMI(
          *MBB, I, MI->getDebugLoc(),
          TII->get(JVMMCOpcodeUtils::GetLDC(MRI->getRegClass(MORHS.getReg()))))
          .addDef(NewReg)
          .addImm(ConstNode->getOperand(1).getImm());
      AddToDeferredEraseSet(ConstNode);
    } else {
      unsigned LoadOpcode =
          JVMMCOpcodeUtils::GetLoadOpcode(MRI->getRegClass(MORHS.getReg()));
      BuildMI(*MBB, I, MI->getDebugLoc(), TII->get(LoadOpcode))
          .addDef(NewReg)
          .addUse(MORHS.getReg());
    }

    // Insert a STORE for LHS of a COPY instruction.
    MachineOperand MOLHS = MI->getOperand(0);
    unsigned StoreOpcode =
        JVMMCOpcodeUtils::GetStoreOpcode(MRI->getRegClass(MOLHS.getReg()));
    BuildMI(*MBB, I, MI->getDebugLoc(), TII->get(StoreOpcode))
        .addDef(MOLHS.getReg())
        .addUse(NewReg);

    AddToDeferredEraseSet(MI);
  }
}

/// ExitScopeIfDone - Destroy scope for the MBB that corresponds to the given
/// dominator tree node if its a leaf or all of its children are done. Walk
/// up the dominator tree to destroy ancestors which are now done.
void JVMCopyElision::ExitScopeIfDone(
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

void JVMCopyElision::PerformCopyElision(MachineDomTreeNode *Node) {
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
    ProcessCopyElisionBlock(MBB);
    // If it's a leaf node, it's done. Traverse upwards to pop ancestors.
    ExitScopeIfDone(Node, OpenChildren);
  }
  Cleanup();
}

bool JVMCopyElision::runOnMachineFunction(MachineFunction &MFN) {
  MF = &MFN;
  TII = static_cast<const JVMInstrInfo *>(MF->getSubtarget().getInstrInfo());
  TRI = static_cast<const JVMRegisterInfo *>(
      MF->getSubtarget().getRegisterInfo());
  MRI = &MF->getRegInfo();
  DT = &getAnalysis<MachineDominatorTree>();

  DEBUG(dbgs() << "\n******** JVM COPY elision ********\n");
  PerformCopyElision(DT->getRootNode());
  return true;
}

namespace llvm {
FunctionPass *createJVMCopyElision() { return new JVMCopyElision(); }
} // namespace llvm
