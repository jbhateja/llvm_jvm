//===------------- JVMCodeLetExtractor.cpp---------------------------------===//
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

#include "llvm/CodeGen/JVMCodeExtractor.h"
#include "llvm/Analysis/LoopInfo.h"

using namespace llvm;
using namespace std;

#define DEBUG_TYPE "jvm-code-extractor"

namespace llvm {

bool CodeLetExtractor::IsExtractableBeyondThreashold(Function **F) {
  return true;
}

void CodeLetExtractor::doExtraction() { return; }
} // namespace llvm
