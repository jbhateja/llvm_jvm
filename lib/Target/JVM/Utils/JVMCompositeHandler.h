//===------------- JVMCompositeHandler.h ----------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
// This file contains utility class to disintegrate structure type to its
// primitive constituents scalar/arrays.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_JVM_JVMCOMPOSITEHANDLER_H
#define LLVM_LIB_TARGET_JVM_JVMCOMPOSITEHANDLER_H

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

// Struct types can be passed by argument,
// created over stack (alloca) and dynamically allocated.
// Globals are unsupported.
class CompositeHandler : public InstVisitor<CompositeHandler, void> {
public:
  using ValueSet = DenseSet<Value *>;
  using TypeVec = SmallVector<Type *, 16>;
  using ValueVec = SmallVector<Value *, 16>;
  using TypeDefMap = DenseMap<Type *, Type *>;
  using TypeValueMap = DenseMap<Type *, ValueVec>;
  using TypeIsBlastedMap = DenseMap<Type *, bool>;
  using TypeNumElemsMap = DenseMap<Type *, int>;
  using ValueValueVecMap = DenseMap<Value *, ValueVec>;

  CompositeHandler() {}
  CompositeHandler(Function &F) : Func(&F) {}

  void ProcessFunction();

  void visitAlloca(AllocaInst &I);
  void visitCallInst(CallInst &I);
  void visitValue(Value *V, Type *Ty);

  void setFunction(Function &F) { Func = &F; }

  void dump();

  TypeDefMap &getTypeDefMap() { return typeDefs; }
  TypeValueMap &getTypeValueMap() { return typeValues; }

private:
  bool isBlastedType(Type *Ty);
  bool canFlattenType(Type *Ty);
  void flattenTypes();
  Type *flattenType_core(LLVMContext &Cxt, Type *Ty);

  int getNumElements(Type *Ty, bool AccessMemFormFlatStruct);

  GetElementPtrInst *extractIndices(Type *, GetElementPtrInst *Elm,
                                    ValueVec &IList, ValueSet &Arr,
                                    bool AccessMemFormFlatStruct);

  Value *getTyIndexInFlattenedType(Type *OTy, GetElementPtrInst *GI,
                                   bool AccessMemFormFlatStruct);
  Value *getTyIndexInFlattenedType_core(Function *Func, Value *Idx,
                                        GetElementPtrInst *GI,
                                        bool AccessMemFormFlatStruct);
  void doTypeCollection();
  void doAllocations();
  void doReplacements();

  void copyMemory(Value *Dst, Value *Src, Type *Ty, ValueVec &ValArr,
                  IRBuilder<> &IRB);

  void replaceMemoryIntrinsics(Type *Ty, CallInst *CI, ValueSet &LazyRemovalSet,
                               ValueVec &ValArr);

  void replaceAllUsesWithUnwrapped(Value *Old, Value *New);

  void replaceGetElementPtrChain(Type *Ty, GetElementPtrInst *V,
                                 ValueSet &LazyDelArr, ValueVec &ValArr);

  void replaceCastChainAndMemIntrinsics(Type *Ty, BitCastInst *V,
                                        ValueSet &LazyRemovalSet,
                                        ValueVec &ValArr);

  void replaceValue(Type *Ty, Value *V, bool RemoveVal,
                    ValueSet &LazyRemovalSet);

  bool AreAllUsesConstantIndexedReferences(Value *I);

  TypeDefMap typeDefs;
  TypeValueMap typeValues;
  TypeNumElemsMap typeNumElemCache;
  ValueValueVecMap objFlatObjArr;
  TypeIsBlastedMap typeIsBlasted;
  Function *Func;
};
} // namespace llvm

#endif
