//===-- JVMMCAsmInfo.h - JVM asm properties -----*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
///
/// \file
/// \brief This file contains the declaration of the JVMMCAsmInfo class.
///
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_JVM_MCTARGETDESC_JVMMCASMINFO_H
#define LLVM_LIB_TARGET_JVM_MCTARGETDESC_JVMMCASMINFO_H

#include "llvm/MC/MCAsmInfoELF.h"
#include "llvm/MC/MCAsmInfoWasm.h"

namespace llvm {

class Triple;

class JVMMCAsmInfo final : public MCAsmInfo {
public:
  explicit JVMMCAsmInfo(const Triple &T);
  ~JVMMCAsmInfo() override;

  bool isValidUnquotedName(StringRef Name) const override { return true; }
};
} // end namespace llvm

#endif
