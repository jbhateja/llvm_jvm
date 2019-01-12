//===-- JVMTargetObjectFile.h - JVM Object Info -*- C++ -*-===//
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
/// TargetLoweringObjectFile.
///
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_JVM_JVMTARGETOBJECTFILE_H
#define LLVM_LIB_TARGET_JVM_JVMTARGETOBJECTFILE_H

#include "llvm/CodeGen/TargetLoweringObjectFileImpl.h"

namespace llvm {

class JVMTargetObjectFile final : public TargetLoweringObjectFileELF {
public:
  JVMTargetObjectFile() = default;

  void Initialize(MCContext &Ctx, const TargetMachine &TM) override;
};
} // end namespace llvm

#endif
