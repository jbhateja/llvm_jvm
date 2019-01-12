//===----------------------  JVMMICommonUtils.cpp -------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//                     JVM Common MI Utility functions.
//===----------------------------------------------------------------------===//

#include "JVMMICommonUtils.h"
#include "JVMInstrInfo.h"
#include "JVMMachineFunctionInfo.h"
#include "JVMMCOpcodeUtils.h"
#include "llvm/MC/MCInstrDesc.h"
#include "llvm/MC/MCRegisterInfo.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/raw_ostream.h"

using namespace llvm;

namespace llvm {

bool isConstantOpcode(unsigned Opcode, bool &isInt) {
  switch (Opcode) {
  default:
    return false;
  case JVM::CONST_INT8:
  case JVM::CONST_INT16:
  case JVM::CONST_INT32:
  case JVM::CONST_INT64: {
    isInt = true;
    return true;
  }
  case JVM::CONST_FLOAT32:
  case JVM::CONST_FLOAT64:
    return true;
  }
}

bool HasConstantDriver(MachineRegisterInfo *MRI, const MachineOperand &MO,
                       bool &isInt, MachineInstr **ConstDriver) {
  assert(MO.isReg() && TargetRegisterInfo::isVirtualRegister(MO.getReg()) &&
         "Operand is not a virtual register.");

  if (!MRI->hasOneDef(MO.getReg()))
    return false;

  MachineInstr *Driver = MRI->getVRegDef(MO.getReg());
  unsigned Opcode = Driver->getOpcode();
  if (isConstantOpcode(Opcode, isInt)) {
    *ConstDriver = Driver;
    MachineOperand ConstNode = Driver->getOperand(1);
    assert((ConstNode.isImm() || ConstNode.isFPImm()) &&
           "Illegal ConstNode type.");
    return true;
  } else
    return false;
}
} // namespace llvm
