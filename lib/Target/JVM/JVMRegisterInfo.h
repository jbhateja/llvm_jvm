// JVMRegisterInfo.h - JVM Register Information Impl -*- C++ -*-
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
///
/// \file
/// \brief This file contains the JVM implementation of the
/// JVMRegisterInfo class.
///
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_JVM_JVMREGISTERINFO_H
#define LLVM_LIB_TARGET_JVM_JVMREGISTERINFO_H

#define GET_REGINFO_HEADER
#include "JVMGenRegisterInfo.inc"

namespace llvm {

class MachineFunction;
class RegScavenger;
class TargetRegisterClass;
class Triple;

class JVMRegisterInfo final : public JVMGenRegisterInfo {
  const Triple &TT;

public:
  explicit JVMRegisterInfo(const Triple &TT);

  const MCPhysReg *getCalleeSavedRegs(const MachineFunction *MF) const override;

  void eliminateFrameIndex(MachineBasicBlock::iterator MI, int SPAdj,
                           unsigned FIOperandNum,
                           RegScavenger *RS = nullptr) const override;

  // Debug information queries.
  unsigned getFrameRegister(const MachineFunction &MF) const override;

  BitVector getReservedRegs(const MachineFunction &MF) const override;

  const TargetRegisterClass *
  getPointerRegClass(const MachineFunction &MF,
                     unsigned Kind = 0) const override;
};
} // end namespace llvm

#endif
