//===------------- JVMCastHandler.h ----------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
// This file contains utility class to handle bitcast (re-interpret) cast
// instruction.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_JVM_JVMCASTHANDLER_H
#define LLVM_LIB_TARGET_JVM_JVMCASTHANDLER_H

#include "llvm-c/Types.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/FoldingSet.h"
#include "llvm/ADT/PointerIntPair.h"
#include "llvm/ADT/PostOrderIterator.h"
#include "llvm/ADT/SmallSet.h"
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

// Following class does the necessary transformations needed
// for handling re-interpret casting for JVM.
class CastHandler : public InstVisitor<CastHandler, void> {
public:
  using TypeIndexPair = std::pair<Type *, Value *>;
  using TypeIndexVec = SmallVector<TypeIndexPair, 4>;

  using LazyDeletionSet = DenseSet<User *>;
  using SrcMemOpOffset = DenseMap<User *, Value *>;

  CastHandler() {}
  CastHandler(Function &F) : Func(&F) {}
  void setFunction(Function &F) { Func = &F; }

  void ProcessFunction();
  void visitBitCastInst(BitCastInst &I);
  void dump();

private:
  Value *GenMask(IRBuilder<> &IRB, Type *SrcTy, Type *DestTy, Value *Offset,
                 bool isLoad);

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
                                Value *Offset, unsigned Align);

  Value *CreateGEP(IRBuilder<> &IRB, Value *I, unsigned i);

  void ProcessMemOpWRTCompositeSource(Value *SrcMem,
                                      SrcMemOpOffset &MemOpOffsetMap,
                                      Type *DestTy, unsigned Align);

  void ProcessArrays(Type *SrcTy, Type *DestTy, Value *Offset, Value *SrcMem,
                     BitCastInst &I);

  void ProcessStructures(Type *SrcTy, Type *DestTy, Value *Offset,
                         Value *SrcMem, BitCastInst &I);

  void AddToLazyDeletionSet(User *);
  void ProcessLazyDeletionSet();
  void PopulateEraseUsers(User *I, SetVector<User *> &Users);

  Function *Func;
  Type *TargetIdxTy;
  LazyDeletionSet DeletionSet;
};
} // namespace llvm

#endif
