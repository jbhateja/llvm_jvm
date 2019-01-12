//===-- JVMRegisterInfo.cpp - JVM Register Information ----===//
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
/// TargetRegisterInfo class.
///
//===----------------------------------------------------------------------===//

#include "JVMRegisterInfo.h"
#include "JVMFrameLowering.h"
#include "JVMInstrInfo.h"
#include "JVMMachineFunctionInfo.h"
#include "JVMSubtarget.h"
#include "MCTargetDesc/JVMMCTargetDesc.h"
#include "llvm/ADT/BitVector.h"
#include "llvm/CodeGen/MachineFrameInfo.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/MachineRegisterInfo.h"
#include "llvm/CodeGen/TargetFrameLowering.h"
#include "llvm/IR/Function.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Target/TargetOptions.h"

using namespace llvm;

#define DEBUG_TYPE "jvm-reg-info"

#define GET_REGINFO_TARGET_DESC
#include "JVMGenRegisterInfo.inc"

JVMRegisterInfo::JVMRegisterInfo(const Triple &TT)
    : JVMGenRegisterInfo(0), TT(TT) {}

const MCPhysReg *
JVMRegisterInfo::getCalleeSavedRegs(const MachineFunction *) const {
  static const MCPhysReg CalleeSavedRegs[] = {0};
  return CalleeSavedRegs;
}

void JVMRegisterInfo::eliminateFrameIndex(MachineBasicBlock::iterator II,
                                          int SPAdj, unsigned FIOperandNum,
                                          RegScavenger * /*RS*/) const {}

unsigned JVMRegisterInfo::getFrameRegister(const MachineFunction &MF) const {
  // No SP and FP for JVM yet.
  return 0;
}

const TargetRegisterClass *
JVMRegisterInfo::getPointerRegClass(const MachineFunction &MF,
                                    unsigned Kind) const {
  assert(Kind == 0 && "Only one kind of pointer on JVM");
  if (MF.getSubtarget<JVMSubtarget>().hasAddr64())
    return &JVM::I64RegClass;
  return &JVM::I32RegClass;
}

BitVector JVMRegisterInfo::getReservedRegs(const MachineFunction &MF) const {
  // Marking all registers as reserved since no register allocation
  // needed for JVM.
  BitVector resRegs(getNumRegs());
  return resRegs.set();
}
