//===------------- JVMCodeExtractorr.h ------------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_CODEGEN_JVM_JVMCODEEXTRACTOR_H
#define LLVM_LIB_CODEGEN_JVM_JVMCODEEXTRACTOR_H

#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Instructions.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/Error.h"
#include "llvm/CodeGen/JVMCandidateChecker.h"
#include "llvm/Transforms/Utils/CodeExtractor.h"

namespace llvm {

// Following class visits function's IR and creates a set of
// intervals, it then checks if each region can be extracted into
// a function, while doing so it uses CandidateChecker to
// validate if the extracted function can be a translation
// candidate, all the operations are performed over the initial
// function clone generated and it computes what %age of the
// function can be translated to VM code.

// Original function call is replaced by new function and CodeLets
// only when total %age of function translatable to VM code goes
// above user specified threshold.

// a valid candidate for translation.
class CodeLetExtractor {
public:
  using FuncVecTy = SmallVector<Function *, 8>;

  CodeLetExtractor() {}
  CodeLetExtractor(Function &F) : Func(&F) {}

  void setFunction(Function &F) { Func = &F; }
  FuncVecTy getFunctions() { return CodeLets; }

  void doExtraction();
  bool IsExtractableBeyondThreashold(Function **F);

private:
  Function *Func;
  FuncVecTy CodeLets;
  CandidateChecker Checker;
};
} // namespace llvm

#endif
