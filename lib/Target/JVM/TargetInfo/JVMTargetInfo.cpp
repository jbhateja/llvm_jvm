//===-- JVMTargetInfo.cpp - JVM Target Implementation -----===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
///
/// \file
/// \brief This file registers the JVM target.
///
//===----------------------------------------------------------------------===//

#include "MCTargetDesc/JVMMCTargetDesc.h"
#include "llvm/ADT/Triple.h"
#include "llvm/Support/TargetRegistry.h"
using namespace llvm;

#define DEBUG_TYPE "jvm-target-info"

Target &llvm::getTheJVMTarget32() {
  static Target TheJVMTarget32;
  return TheJVMTarget32;
}
Target &llvm::getTheJVMTarget64() {
  static Target TheJVMTarget64;
  return TheJVMTarget64;
}

extern "C" void LLVMInitializeJVMTargetInfo() {
  RegisterTarget<Triple::jvm32> X(getTheJVMTarget32(), "jvm32", "JVM 32-bit",
                                  "JVM");
  RegisterTarget<Triple::jvm64> Y(getTheJVMTarget64(), "jvm64", "JVM 64-bit",
                                  "JVM");
}
