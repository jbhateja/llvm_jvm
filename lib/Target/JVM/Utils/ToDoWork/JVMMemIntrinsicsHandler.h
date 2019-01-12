//===------------- JVMMemIntrinsicsHandler.h -------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===-----------------------------------------------------------------------===//
// This file contains utility class to handle memory intrinsic instructions.
// Memory intrinsics (llvm.memcpy, llvm.memmove and llvm.memset) are expanded
// to assginments from source to destination. 
// Size of memmory operations can be constant / variable. 
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_JVM_JVMMEMINTRINSICSHANDLER_H
#define LLVM_LIB_TARGET_JVM_JVMMEMINTRINSICSHANDLER_H

#include "llvm-c/Types.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/FoldingSet.h"
#include "llvm/ADT/PointerIntPair.h"
#include "llvm/ADT/PostOrderIterator.h"
#include "llvm/ADT/SmallSet.h"
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

// Following class does the necessary transformations needed
// for handling re-interpret casting for JVM.
class MemIntrinsicsHandler : public InstVisitor<MemIntrinsicsHandler, void> {
public:
  using TypeIndexPair = std::pair<Type *, Value *>;
  using TypeIndexVec = SmallVector<TypeIndexPair, 4>;

  using LazyDeletionSet = DenseSet<User *>;
  using SrcMemOpOffset = DenseMap<User *, Value *>;

  MemIntrinsicsHandler() {}
  MemIntrinsicsHandler(Function &F) : Func(&F) {}
  void setFunction(Function &F) { Func = &F; }

  void ProcessFunction();
  void visitCallInst(CallInst &I);
  void dump();

private:
  Value *GenMask(IRBuilder<> &IRB, Type *SrcTy, Type *DestTy, Value *Offset,
                 bool isLoad);

  Value *GenMask(IRBuilder<> &IRB, unsigned SrcSz, unsigned DestSz,
                 Value *Offset, bool IsLoad);

  void ProcessMemOpWRTSourcePrimitive(Value *SrcMem,
                                      SrcMemOpOffset &MemOpOffsetMap,
                                      Type *DestTy);

  bool ProcessUser(SrcMemOpOffset &MemOpOffetMap, User &I, Value *Offset,
                   bool isStruct);

  bool ProcessGEPChain(SrcMemOpOffset &MemOpOffetMap, GetElementPtrInst &I,
                       Value *Offset, bool isStruct);

  void ProcessPrimitive(Type *SrcTy, Type *DestTy, Value *Offset, Value *SrcMem,
                        BitCastInst &I);

  void ProcessLoadStorePrimtive(IRBuilder<> &IRB, Value *SrcMem, Value *MemOp,
                                Value *Offset);

  unsigned GetStructElmIndexAtOffset(StructType *STy, unsigned Offset,
                                     unsigned &PadOffset);

  void GetIndicesForCompositeElemAccess(IRBuilder<> &IRB,
                                        SmallVectorImpl<Value *> &Indices,
                                        Type *CompType, Value **Offset);

  Value *CreateCompositeElemAccessGEP(IRBuilder<> &IRB, Value *Mem,
                                      Type *CompType, Value **Offset);

  Value *CreateGEP(IRBuilder<> &IRB, Value *I, unsigned i);

  Value *GetNextGEP(IRBuilder<> &IRB, Value *V);

  void ProcessMemOpWRTCompositeSource(Value *SrcMem,
                                      SrcMemOpOffset &MemOpOffsetMap,
                                      Type *DestTy);

  void ProcessArrays(Type *SrcTy, Type *DestTy, Value *Offset, Value *SrcMem,
                     BitCastInst &I);

  void ProcessStructures(Type *SrcTy, Type *DestTy, Value *Offset,
                         Value *SrcMem, BitCastInst &I);

  void AddToLazyDeletionSet(User *);
  void ProcessLazyDeletionSet();
  void PopulateEraseUsers(User *I, SmallVectorImpl<User *> &Users);

  Function *Func;
  Type *TargetIdxTy;
  LazyDeletionSet DeletionSet;
};
} // namespace llvm

#endif
