//===------------- JVMLowerMemoryIntrinsics.h------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
// This file contains utility pass to lower memory intrinsic instructions.
// Memory intrinsics (llvm.memcpy, llvm.memmove and llvm.memset) are expanded
// to assginments from source to destination. Size of memmory operations can
// be constant / variable.
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_JVM_JVMMEMINTRINSICSHANDLER_H
#define LLVM_LIB_TARGET_JVM_JVMMEMINTRINSICSHANDLER_H

#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/FoldingSet.h"
#include "llvm/ADT/PointerIntPair.h"
#include "llvm/ADT/PostOrderIterator.h"
#include "llvm/ADT/SetVector.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Analysis/CodeMetrics.h"
#include "llvm/Analysis/TargetTransformInfo.h"
#include "llvm/IR/CFG.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/GetElementPtrTypeIterator.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/InstVisitor.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/PassManager.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/Error.h"

using namespace llvm;

namespace llvm {

class JVMLowerMemoryIntrinsics : public FunctionPass {
public:
  using LazyDeletionSet = DenseSet<User *>;

  static char ID; // Pass identification, replacement for typeid
  JVMLowerMemoryIntrinsics() : FunctionPass(ID) {}

  bool runOnFunction(Function &F) override {
    Func = &F;
    return ProcessFunction();
  }

private:
  void ResetOffsets();
  void dump();

  bool ProcessFunction();
  void ProcessLazyDeletionSet();
  void AddToLazyDeletionSet(User *Elm);
  void ProcessIntrinsics(SmallVectorImpl<MemIntrinsic *> &WorkList);

  void PopulateEraseUsers(User *I, SetVector<User *> &Users);

  void OptMemTransferStruct(IRBuilder<> &IRB, Value *Src, Value *Dst,
                            Value *Langth);

  void OptimizeMemCopy(IRBuilder<> &IRB, MemIntrinsic *Inst);

  void OptMemTransferSerial(IRBuilder<> &IRB, Value *Src, Value *SrcOffset,
                            Value *Dst, Value *DstOffset, Value *Langth,
                            MemIntrinsic *Inst);

  unsigned OptMemTransferScalar(IRBuilder<> &IRB, Value *Src, Value *Dst,
                                MemIntrinsic *Inst);

  void OptMemTransferArray(IRBuilder<> &IRB, Value *Src, Value *SrcOffset,
                           Value *Dst, Value *DstOffset, Value *Langth,
                           MemIntrinsic *Inst);

  void OptimizeMemSet(Value *Src, Value *Dst, Value *Length);

  bool CheckForPartialElementAccess(Type *Ty, unsigned Offset);

  bool IsPartialElementAccess(Value *V);

  void SetInitGEPs(Value *Src, Value *SrcOffset, Value *Dst, Value *DstOffset);

  Value *PrepareValueForMemScalar(IRBuilder<> &IRB, Value *Val, Type *ToType);

  Value *PrepareValueForMemSet(IRBuilder<> &IRB, Value *Val, Type *ToType);

  Type *GetTypeAtOffset(Type *Ty, Value **Offset);

  Value *GetNextSrcGep(IRBuilder<> &IRB);

  Value *GetNextDstGep(IRBuilder<> &IRB);

  Value *GetNextGep(IRBuilder<> &IRB, Value *V, bool IsSrcGep);

  Value *GetDstMask(unsigned WindowSz);

  Value *TransformMemMoveToCpy(Value *I);

  Value *GetUnwrappedValue(Value *V);

  uint64_t CurrDstOffset = {0};
  uint64_t CurrSrcOffset = {0};
  Value *CurrSrc = {nullptr};
  Value *CurrDst = {nullptr};

  Function *Func;
  Type *TargetIdxTy;
  LazyDeletionSet DeletionSet;
};
} // namespace llvm

#endif
