//===-- JVMTargetObjectFile.cpp - JVM Object Info ---------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
///
/// \file
/// \brief This file defines the functions of the JVM-specific subclass
/// of TargetLoweringObjectFile.
///
//===----------------------------------------------------------------------===//

#include "JVMTargetObjectFile.h"
#include "JVMTargetMachine.h"
using namespace llvm;

void JVMTargetObjectFile::Initialize(MCContext &Ctx, const TargetMachine &TM) {
  // TODO: Replace ELF with JVM Class object format.
  TargetLoweringObjectFileELF::Initialize(Ctx, TM);
  InitializeELF(TM.Options.UseInitArray);
}
