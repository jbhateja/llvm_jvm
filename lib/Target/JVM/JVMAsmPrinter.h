// JVMAsmPrinter.h - JVM implementation of AsmPrinter-*- C++ --------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_JVM_JVMASMPRINTER_H
#define LLVM_LIB_TARGET_JVM_JVMASMPRINTER_H

#include "JVMMachineFunctionInfo.h"
#include "JVMSubtarget.h"
#include "llvm/CodeGen/AsmPrinter.h"
#include "llvm/IR/Constant.h"
#include "llvm/IR/Constants.h"
#include "llvm/MC/MCStreamer.h"
#include "llvm/Target/TargetMachine.h"

namespace llvm {
class MCSymbol;
class JVMTargetStreamer;
class JVMMCInstLower;

class LLVM_LIBRARY_VISIBILITY JVMAsmPrinter final : public AsmPrinter {
public:
  using FieldTypeIndexVec = SmallVector<std::pair<Type *, unsigned>, 4>;
  using ConstantVals = SmallVector<const Constant *, 8>;

  explicit JVMAsmPrinter(TargetMachine &TM,
                         std::unique_ptr<MCStreamer> Streamer)
      : AsmPrinter(TM, std::move(Streamer)), Subtarget(nullptr), MRI(nullptr),
        MFI(nullptr) {}

  StringRef getPassName() const override { return "JVM Assembly Printer"; }

  const JVMSubtarget &getSubtarget() const { return *Subtarget; }

  //===------------------------------------------------------------------===//
  // MachineFunctionPass Implementation.
  //===------------------------------------------------------------------===//

  bool runOnMachineFunction(MachineFunction &MF) override {
    Subtarget = &MF.getSubtarget<JVMSubtarget>();
    MRI = &MF.getRegInfo();
    MFI = MF.getInfo<JVMFunctionInfo>();
    OutStreamer->SetFunctionInfo(MFI->getDescVector());
    return AsmPrinter::runOnMachineFunction(MF);
  }

  //===------------------------------------------------------------------===//
  // AsmPrinter Implementation.
  //===------------------------------------------------------------------===//
  void EmitStartOfAsmFile(Module &M) override;
  void EmitEndOfAsmFile(Module &M) override;

  void EmitFunctionBodyStart() override;
  void EmitFunctionBodyEnd() override;

  void EmitGlobalVariable(const GlobalVariable *GV) override;

  void EmitInstruction(const MachineInstr *MI) override;

  const MCExpr *lowerConstant(const Constant *CV) override;
  bool PrintAsmOperand(const MachineInstr *MI, unsigned OpNo,
                       unsigned AsmVariant, const char *ExtraCode,
                       raw_ostream &OS) override;

  // void EmitFunctionHeader() override {}

  void EmitFunctionEntryLabel() override {}

  JVMTargetStreamer *getTargetStreamer();

private:
  void EmitClassDefinitions();
  std::string EmitTypeString(Type *Ty);
  void EmitFieldAllocation(std::string &FieldInit, Type *Ty,
                           std::string ClassName, unsigned FieldID);
  void EmitClassFields(std::string &StrBuf, StructType *STy,
                       FieldTypeIndexVec &CompFields);
  std::string EmitClassDefinition(Type *Ty, std::string ClassName);

  void EmitFieldInitialization(std::string &AsmBuf, Type *PTy, Type *Ty,
                               unsigned &ConstIdx, ConstantVals &CVec,
                               unsigned FieldID, bool IsTopLevelObj);

  void PopulateConstValVector(const Constant *Init, ConstantVals &CVec);

  std::string GetStoreString(Type *Ty);

  std::string GetLoadString(Type *Ty);

  const JVMSubtarget *Subtarget;
  const MachineRegisterInfo *MRI;
  JVMFunctionInfo *MFI;
};
} // end namespace llvm

#endif
