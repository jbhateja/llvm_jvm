//===-- JVMMCAsmInfo.cpp - JVM asm properties -------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
///
/// \file
/// \brief This file contains the declarations of the JVMMCAsmInfo
/// properties.
///
//===----------------------------------------------------------------------===//

#include "JVMMCAsmInfo.h"
#include "llvm/ADT/Triple.h"
#include "llvm/Support/CommandLine.h"

using namespace llvm;

#define DEBUG_TYPE "jvm-mc-asm-info"

static cl::opt<bool>
    EnableJVMAssembler("enable-jvm-assembler", cl::NotHidden,
                       cl::desc("Assembles JVM asm using Jasmin assembler"));

JVMMCAsmInfo::~JVMMCAsmInfo() {}

JVMMCAsmInfo::JVMMCAsmInfo(const Triple &T) {
  CodePointerSize = CalleeSaveStackSlotSize = T.isArch64Bit() ? 8 : 4;

  GlobalDirective = "";
  UseDataRegionDirectives = true;
  HasSingleParameterDotFile = false;
  HasDotTypeDotSizeDirective = false;

  if (EnableJVMAssembler) {
    EnableInMemoryAsmEmit = true;
    AsmMemoryBuffer = new std::string("");
  }

  AlignmentIsInBytes = false;
  COMMDirectiveAlignmentIsInBytes = false;
  LCOMMDirectiveAlignmentType = LCOMM::Log2Alignment;

  // For now, JVM does not support exceptions.
  ExceptionsType = ExceptionHandling::None;

  // TODO: UseIntegratedAssembler?
}
