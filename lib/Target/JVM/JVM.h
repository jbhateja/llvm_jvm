//===-- JVM.h - Top-level interface for JVM  ----*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
///
/// \file
/// \brief This file contains the entry points for global functions defined in
/// the LLVM JVM back-end.
///
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_JVM_JVM_H
#define LLVM_LIB_TARGET_JVM_JVM_H

#include "llvm/PassRegistry.h"
#include "llvm/Support/CodeGen.h"

namespace llvm {

class JVMTargetMachine;
class ModulePass;
class FunctionPass;

// ISel and immediate followup passes.
FunctionPass *createJVMISelDag(JVMTargetMachine &TM,
                               CodeGenOpt::Level OptLevel);
FunctionPass *createJVMLoadStoreEliminationOpt();
FunctionPass *createJVMLowerMemoryIntrinsics();
FunctionPass *createJVMPreAllocationFixups();
FunctionPass *createJVMOffsetAllocator();
FunctionPass *createJVMPostAllocationFixups();
FunctionPass *createJVMCopyElision();
FunctionPass *createX2JavaPass();
FunctionPass *createJVMIRDecorator();
} // end namespace llvm

#endif
