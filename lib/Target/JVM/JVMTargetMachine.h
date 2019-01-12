// JVMTargetMachine.h - Define TargetMachine for JVM -*- C++ -*-
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
///
/// \file
/// \brief This file declares the JVM-specific subclass of
/// TargetMachine.
///
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_JVM_JVMTARGETMACHINE_H
#define LLVM_LIB_TARGET_JVM_JVMTARGETMACHINE_H

#include "JVMSubtarget.h"
#include "llvm/Target/TargetMachine.h"

namespace llvm {

class JVMTargetMachine final : public LLVMTargetMachine {
  std::unique_ptr<TargetLoweringObjectFile> TLOF;
  mutable StringMap<std::unique_ptr<JVMSubtarget>> SubtargetMap;

public:
  JVMTargetMachine(const Target &T, const Triple &TT, StringRef CPU,
                   StringRef FS, const TargetOptions &Options,
                   Optional<Reloc::Model> RM, Optional<CodeModel::Model> CM,
                   CodeGenOpt::Level OL, bool JIT);

  ~JVMTargetMachine() override;

  const JVMSubtarget *getSubtargetImpl(const Function &F) const override;

  void doCommandLineValidation() override;

  // Pass Pipeline Configuration
  TargetPassConfig *createPassConfig(PassManagerBase &PM) override;

  TargetLoweringObjectFile *getObjFileLowering() const override {
    return TLOF.get();
  }
};
} // end namespace llvm

#endif
