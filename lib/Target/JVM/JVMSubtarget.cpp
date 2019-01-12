//===-- JVMSubtarget.cpp - JVM Subtarget Information ------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
///
/// \file
/// \brief This file implements the JVM-specific subclass of
/// TargetSubtarget.
///
//===----------------------------------------------------------------------===//

#include "JVMSubtarget.h"
#include "JVMInstrInfo.h"
#include "MCTargetDesc/JVMMCTargetDesc.h"
#include "llvm/Support/TargetRegistry.h"
using namespace llvm;

#define DEBUG_TYPE "jvm-subtarget"

#define GET_SUBTARGETINFO_CTOR
#define GET_SUBTARGETINFO_TARGET_DESC
#include "JVMGenSubtargetInfo.inc"

JVMSubtarget &JVMSubtarget::initializeSubtargetDependencies(StringRef FS) {
  // Determine default and user-specified characteristics
  if (CPUString.empty())
    CPUString = "jvm";
  return *this;
}

JVMSubtarget::JVMSubtarget(const Triple &TT, const std::string &CPU,
                           const std::string &FS, const TargetMachine &TM)
    : JVMGenSubtargetInfo(TT, CPU, FS), CPUString(CPU), TargetTriple(TT),
      FrameLowering(), InstrInfo(initializeSubtargetDependencies(FS)), TSInfo(),
      TLInfo(TM, *this) {}

bool JVMSubtarget::enableMachineScheduler() const {
  // Disable the MachineScheduler for now. Even with ShouldTrackPressure set and
  // enableMachineSchedDefaultSched overridden, it appears to have an overall
  // negative effect for the kinds of register optimizations we're doing.
  return false;
}

bool JVMSubtarget::useAA() const { return true; }
