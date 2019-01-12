//===------------------------  JVMMCOpcodeUtils.h --------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//  Utility class for getting instruction opcodes of required type and size.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_JVM_JVMMCOPCODEUTILS_H
#define LLVM_LIB_TARGET_JVM_JVMMCOPCODEUTILS_H

#include "llvm/CodeGen/MachineRegisterInfo.h"

using namespace llvm;

class JVMMCOpcodeUtils {
public:
  static unsigned GetLoadOpcode(const TargetRegisterClass *RC) {
    if (RC == &JVM::I8RegClass)
      return JVM::ILOAD8;
    else if (RC == &JVM::I16RegClass)
      return JVM::ILOAD16;
    else if (RC == &JVM::I32RegClass)
      return JVM::ILOAD32;
    else if (RC == &JVM::I64RegClass)
      return JVM::ILOAD64;
    else
      llvm_unreachable("No LOAD opcode for corresponding register class.");
    return JVM::NOP;
  }

  static const TargetRegisterClass * GetConstantRC(unsigned Opcode) {
    switch (Opcode) {
      default : llvm_unreachable("Unknown constant opcode");
      case JVM::CONST_INT8:  return &JVM::I8RegClass;
      case JVM::CONST_INT16: return &JVM::I16RegClass;
      case JVM::CONST_INT32: return &JVM::I32RegClass;
      case JVM::CONST_INT64: return &JVM::I64RegClass;
    }
  }

  static unsigned GetStoreOpcode(const TargetRegisterClass *RC) {
    if (RC == &JVM::I8RegClass)
      return JVM::ISTORE8;
    else if (RC == &JVM::I16RegClass)
      return JVM::ISTORE16;
    else if (RC == &JVM::I32RegClass)
      return JVM::ISTORE32;
    else if (RC == &JVM::I64RegClass)
      return JVM::ISTORE64;
    else
      llvm_unreachable("No STORE opcode for corresponding register class.");
    return JVM::NOP;
  }

  static unsigned GetLDC(const TargetRegisterClass *RC) {
    if (RC == &JVM::I8RegClass)
      return JVM::CSTLDCI8;
    else if (RC == &JVM::I16RegClass)
      return JVM::CSTLDCI16;
    else if (RC == &JVM::I32RegClass)
      return JVM::CSTLDCI32;
    else if (RC == &JVM::I64RegClass)
      return JVM::CSTLDCI64;
    else if (RC == &JVM::F32RegClass)
      return JVM::LDCF32;
    else if (RC == &JVM::F64RegClass)
      return JVM::LDCF64;
    else
      llvm_unreachable("No LDC opcode for corresponding register class.");
    return JVM::NOP;
  }

  static bool IsLoadInstr(unsigned Opcode) {
    switch(Opcode) {
      default:
        return false;
      case JVM::ILOAD8:
      case JVM::ILOAD16:
      case JVM::ILOAD32:
      case JVM::ILOAD64:
      case JVM::ILOAD8WCH:
      case JVM::ILOAD16WCH:
      case JVM::ILOAD32WCH:
      case JVM::ILOAD64WCH:
        return true;
    }
  }

  static unsigned GetRCConvOpcode(const TargetRegisterClass *LRC, 
                                  const TargetRegisterClass *RRC) { 
     if (RRC == &JVM::I8RegClass) {
       if (LRC == &JVM::I16RegClass)
         return JVM::RCC8to16;
       else if (LRC == &JVM::I32RegClass)
         return JVM::RCC8to32;
       else if (LRC == &JVM::I64RegClass)
         return JVM::RCC8to64;

     } else if (RRC == &JVM::I16RegClass) {
       if (LRC == &JVM::I8RegClass)
         return JVM::RCC16to8;
       else if (LRC == &JVM::I32RegClass)
         return JVM::RCC16to32;
       else if (LRC == &JVM::I64RegClass)
         return JVM::RCC16to64;

     } else if (RRC == &JVM::I32RegClass) {
       if (LRC == &JVM::I8RegClass)
         return JVM::RCC32to8;
       else if (LRC == &JVM::I16RegClass)
         return JVM::RCC32to16;
       else if (LRC == &JVM::I64RegClass)
         return JVM::RCC32to64;
     } else if (RRC == &JVM::I64RegClass) {
       if (LRC == &JVM::I8RegClass)
         return JVM::RCC64to8;
       else if (LRC == &JVM::I16RegClass)
         return JVM::RCC64to16;
     }
     llvm_unreachable("Unhandled register class");
  }
};

#endif
