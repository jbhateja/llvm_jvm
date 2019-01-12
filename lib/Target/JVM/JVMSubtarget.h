//=- JVMSubtarget.h - Define Subtarget for the JVM -*- C++ -*-//
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
/// TargetSubtarget.
///
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_JVM_JVMSUBTARGET_H
#define LLVM_LIB_TARGET_JVM_JVMSUBTARGET_H

#include "JVMFrameLowering.h"
#include "JVMISelLowering.h"
#include "JVMInstrInfo.h"
#include "JVMSelectionDAGInfo.h"
#include "llvm/CodeGen/TargetSubtargetInfo.h"
#include <string>

#define GET_SUBTARGETINFO_HEADER
#include "JVMGenSubtargetInfo.inc"

namespace llvm {

class JVMSubtarget final : public JVMGenSubtargetInfo {
  /// String name of used CPU.
  std::string CPUString;

  /// What processor and OS we're targeting.
  Triple TargetTriple;

  JVMFrameLowering FrameLowering;
  JVMInstrInfo InstrInfo;
  JVMSelectionDAGInfo TSInfo;
  JVMTargetLowering TLInfo;

  /// Initializes using CPUString and the passed in feature string so that we
  /// can use initializer lists for subtarget initialization.
  JVMSubtarget &initializeSubtargetDependencies(StringRef FS);

public:
  /// This constructor initializes the data members to match that
  /// of the specified triple.
  JVMSubtarget(const Triple &TT, const std::string &CPU, const std::string &FS,
               const TargetMachine &TM);

  void ParseSubtargetFeatures(StringRef CPU, StringRef FS);

  const JVMSelectionDAGInfo *getSelectionDAGInfo() const override {
    return &TSInfo;
  }
  const JVMFrameLowering *getFrameLowering() const override {
    return &FrameLowering;
  }
  const JVMTargetLowering *getTargetLowering() const override {
    return &TLInfo;
  }
  const JVMInstrInfo *getInstrInfo() const override { return &InstrInfo; }
  const JVMRegisterInfo *getRegisterInfo() const override {
    return &getInstrInfo()->getRegisterInfo();
  }
  const Triple &getTargetTriple() const { return TargetTriple; }
  bool enableMachineScheduler() const override;
  bool useAA() const override;

  bool hasAddr64() const { return TargetTriple.isArch64Bit(); }
};
} // end namespace llvm

#endif
