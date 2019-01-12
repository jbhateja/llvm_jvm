//===------------------------  JVMMICommonUtils.h --------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//                        Common MI Utility functions.
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_JVM_JVMMICOMMONUTILS_H
#define LLVM_LIB_TARGET_JVM_JVMMICOMMONUTILS_H

#include "llvm/CodeGen/MachineRegisterInfo.h"

namespace llvm {

bool isConstantOpcode(unsigned Opcode, bool &isInt);

bool HasConstantDriver(MachineRegisterInfo *MRI, const MachineOperand &MO,
                       bool &isInt, MachineInstr **ConstDriver);
} // namespace llvm
#endif
