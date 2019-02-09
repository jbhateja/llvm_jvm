//===------------- JVMCandidateChecker.h ----------------------------------===//
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

#ifndef LLVM_LIB_CODEGEN_JVM_JVMCANDIDATECHECKER_H
#define LLVM_LIB_CODEGEN_JVM_JVMCANDIDATECHECKER_H

#include "llvm/ADT/Statistic.h"
#include "llvm/Analysis/TargetTransformInfo.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/GetElementPtrTypeIterator.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/InstVisitor.h"
#include "llvm/IR/Instructions.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/Error.h"

#define IOBUFSIZE 256

namespace llvm {

class MemorySSA;
class MemoryAccess;

// Following class visits function's IR and checks if its
// a valid candidate for translation.
class CandidateChecker : public InstVisitor<CandidateChecker, void> {
public:
  using MemIntrinsicsVector = SmallVector<Value *, 8>;

  CandidateChecker() {}
  CandidateChecker(Function &F) : Func(&F) {}

  bool IsCandidateForTranslation(MemorySSA &MSSA);

  void visitBitCastInst(BitCastInst &I);

  void visitLoad(LoadInst &I);
  void visitStore(StoreInst &I);
  void visitGetElementPtrInst(GetElementPtrInst &I);
  void visitCall(CallInst &I);
  void visitTerminatorInst(Instruction &I);
  void visitAlloca(AllocaInst &I);

  void visitInstruction(Instruction &I);
  void setFunction(Function &F);

  template <typename T> void report_note(T &&, bool Suppress = false);

private:
		Value * GetModifiedMemoryLoc(Value *II);
  bool CheckForStructureRef(Value * V);
  bool CheckInitializedRetVal(PHINode *PHI);
  bool CheckInitializedMemoryRead(MemorySSA &MSSA, LoadInst *LI);
  bool CheckDefList(LoadInst *LI, MemorySSA &MSSA, const MemoryAccess *MA,
                    bool CheckBlockDefs);
  void CheckUninitializedMemoryReads(MemorySSA &MSSA);

  bool CheckTypeCompatibilityForBitCast(Type *SrcTy, Type *DstTy);
  bool CheckValidTypeForTranslation(Type *Ty, bool ByVal = false);
  bool CheckForGlobalRef(Value *V);
  bool CheckFuncSignature();
  int GetNumArrayDimensions(Type *Ty);

  MemIntrinsicsVector MemIntrs;
  bool visitStatus = {true};
  Function *Func;
};
} // namespace llvm

#endif
