//==- JVMMCTargetDesc.h - JVM Target Descriptions -*- C++ -*-=//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
///
/// \file
/// \brief This file provides JVM-specific target descriptions.
///
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_JVM_MCTARGETDESC_JVMMCTARGETDESC_H
#define LLVM_LIB_TARGET_JVM_MCTARGETDESC_JVMMCTARGETDESC_H

#include "llvm/MC/MCInstrDesc.h"
#include "llvm/Support/DataTypes.h"
#include <memory>

namespace llvm {

class MCAsmBackend;
class MCCodeEmitter;
class MCContext;
class MCInstrInfo;
class MCObjectWriter;
class MCSubtargetInfo;
class MVT;
class Target;
class Triple;
class raw_pwrite_stream;

Target &getTheJVMTarget32();
Target &getTheJVMTarget64();

MCAsmBackend *createJVMAsmBackend(const Triple &TT);

namespace JVM {
enum OperandType {
  /// Basic block label in a branch construct.
  OPERAND_BASIC_BLOCK = MCOI::OPERAND_FIRST_TARGET,
  /// Local index.
  OPERAND_LOCAL,
  /// Global index.
  OPERAND_GLOBAL,
  /// 8-bit integer immediates.
  OPERAND_I8IMM,
  /// 16-bit integer immediates.
  OPERAND_I16IMM,
  /// 32-bit integer immediates.
  OPERAND_I32IMM,
  /// 64-bit integer immediates.
  OPERAND_I64IMM,
  /// 32-bit floating-point immediates.
  OPERAND_F32IMM,
  /// 64-bit floating-point immediates.
  OPERAND_F64IMM,
  /// 32-bit unsigned function indices.
  OPERAND_FUNCTION32,
  /// 32-bit unsigned memory offsets.
  OPERAND_OFFSET32,
  /// descriptor immediate.
  OPERAND_DESC,
};
} // end namespace JVM
} // end namespace llvm

// Defines symbolic names for JVM registers. This defines a mapping from
// register name to register number.
//
#define GET_REGINFO_ENUM
#include "JVMGenRegisterInfo.inc"

// Defines symbolic names for the JVM instructions.
//
#define GET_INSTRINFO_ENUM
#include "JVMGenInstrInfo.inc"

#define GET_SUBTARGETINFO_ENUM
#include "JVMGenSubtargetInfo.inc"

#endif
