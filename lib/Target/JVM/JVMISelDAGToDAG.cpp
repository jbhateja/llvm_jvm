//- JVMISelDAGToDAG.cpp - A dag to dag inst selector for JVM -//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
///
/// \file
/// \brief This file defines an instruction selector for the JVM target.
///
//===----------------------------------------------------------------------===//

#include "JVM.h"
#include "JVMTargetMachine.h"
#include "JVMMachineFunctionInfo.h"
#include "MCTargetDesc/JVMMCTargetDesc.h"
#include "llvm/CodeGen/JVMCommonUtils.h"
#include "llvm/CodeGen/SelectionDAGISel.h"
#include "llvm/IR/Function.h" // To access function attributes.
#include "llvm/Support/Debug.h"
#include "llvm/Support/KnownBits.h"
#include "llvm/Support/MathExtras.h"
#include "llvm/Support/raw_ostream.h"
using namespace llvm;

#define DEBUG_TYPE "jvm-isel"

//===--------------------------------------------------------------------===//
/// JVM-specific code to select JVM machine instructions for
/// SelectionDAG operations.
///
namespace {
class JVMDAGToDAGISel final : public SelectionDAGISel {
  /// Keep a pointer to the JVMSubtarget around so that we can make the
  /// right decision when generating code for different targets.
  const JVMSubtarget *Subtarget;
  bool ForCodeSize;

public:
  JVMDAGToDAGISel(JVMTargetMachine &tm, CodeGenOpt::Level OptLevel)
      : SelectionDAGISel(tm, OptLevel), Subtarget(nullptr), ForCodeSize(false) {
  }

  StringRef getPassName() const override { return "JVM Instruction Selection"; }

  bool runOnMachineFunction(MachineFunction &MF) override {
    LLVMContext &Cxt = MF.getFunction().getContext();
    ForCodeSize = MF.getFunction().hasFnAttribute(Attribute::OptimizeForSize) ||
                  MF.getFunction().hasFnAttribute(Attribute::MinSize);
    Subtarget = &MF.getSubtarget<JVMSubtarget>();

    JVMFunctionInfo *MFI = MF.getInfo<JVMFunctionInfo>();
    for (auto &STy : Cxt.getNamedStructTypes())
      MFI->addToTypeNameMap(STy.getValue());

    const Function &F = MF.getFunction();
    if (F.hasFnAttribute(Attribute::JVMInstMethod)) {
      const Module *Mod = F.getParent();
      StringRef ModName = Mod->getName();
      const GlobalVariable *GCStructObj = Mod->getNamedGlobal("ConstantInitStruct");
      assert(GCStructObj && "Global constants structure not present");
      std::string ClassName = ModName.substr(0, ModName.find(".ll"));
      MFI->addToTypeNameMap(GetRootType(GCStructObj->getType()), ClassName);
    }
    
    return SelectionDAGISel::runOnMachineFunction(MF);
  }

  void Select(SDNode *Node) override;

// Include the pieces autogenerated from the target description.
#include "JVMGenDAGISel.inc"

private:
  // add select functions here...
};
} // end anonymous namespace

void JVMDAGToDAGISel::Select(SDNode *Node) {
  // Dump information about the Node being selected.
  LLVM_DEBUG(errs() << "Selecting: ");
  LLVM_DEBUG(Node->dump(CurDAG));
  LLVM_DEBUG(errs() << "\n");

  // If we have a custom node, we already have selected!
  if (Node->isMachineOpcode()) {
    LLVM_DEBUG(errs() << "== "; Node->dump(CurDAG); errs() << "\n");
    Node->setNodeId(-1);
    return;
  }

  // Few custom selection stuff.
  EVT VT = Node->getValueType(0);

  switch (Node->getOpcode()) {
  default:
    break;
    // If we need JVM-specific selection, it would go here.
    (void)VT;
  }

  // Select the default instruction.
  SelectCode(Node);
}

/// This pass converts a legalized DAG into a JVM-specific DAG, ready
/// for instruction scheduling.
FunctionPass *llvm::createJVMISelDag(JVMTargetMachine &TM,
                                     CodeGenOpt::Level OptLevel) {
  return new JVMDAGToDAGISel(TM, OptLevel);
}
