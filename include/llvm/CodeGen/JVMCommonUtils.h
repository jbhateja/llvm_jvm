//===------------- JVMCommonUtils.h----------------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
// This file contains utility functions usable by all the JVM handlers and
// JVM specific CodeGen passes.
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_CODEGEN_JVM_JVMCOMMONUTILS_H
#define LLVM_LIB_CODEGEN_JVM_JVMCOMMONUTILS_H

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
#include "llvm/IR/ConstantFolder.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/GetElementPtrTypeIterator.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/InstVisitor.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/PassManager.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/Error.h"
#include "llvm/Support/MathExtras.h"

using namespace llvm;

namespace llvm {

extern Type *GetRootType(Type *Ty);

extern Type *GetNormalizedRootType(Type *Ty, const DataLayout &DL,
                                   LLVMContext &Cxt);

extern Type *GetTargetIndexType(const DataLayout &DL, LLVMContext &Cxt);

extern Value *GetTypeNormalizedValue(const DataLayout &DL, IRBuilder<> &IRB,
                                     Value *Val, Type *Ty);

extern Value *ComputeOffsetForGEP(IRBuilder<> &IRB, User &I, Value *Offset);

extern Value *GetRootObject(Value *Obj, Value **Offset, bool &HasBitCastInChain,
                            bool ComputeOffset = true, int RecLevel = 0,
                            Instruction *OffsetPlaceHolderInst = nullptr);

extern Value *GetRootObject2(Value *Obj, LLVMContext &Cxt);

extern bool CheckIfLeafElementIsStructureType(Type *Ty);

extern bool CheckIfValuePartOfStructure(Value *V);

extern Value *GetMemorySrc(Value *MI);

extern Value *GenMask(LLVMContext &Cxt, const DataLayout &DL, IRBuilder<> &IRB,
                      unsigned SrcSz, unsigned DestSz, Value *Offset,
                      bool IsLoad);

extern Value *CreateGEPIncLastIndex(IRBuilder<> &IRB, Type *Ty, Value *I,
                                    unsigned i);

extern Value *GetNextGEP(const DataLayout &DL, IRBuilder<> &IRB,
                         Type *TargetIdxTy, Value *V, unsigned &Padding);

extern unsigned GetStructElmIndexAtOffset(StructType *STy, unsigned Offset,
                                          unsigned &PadOffset);

extern void GetIndicesForCompositeElemAccess(const DataLayout &DL,
                                             IRBuilder<> &IRB,
                                             Type *TargetIdxTy,
                                             SmallVectorImpl<Value *> &Indices,
                                             Type *CompType, Value **Offset);

extern Value *CreateCompositeElemAccessGEP(const DataLayout &DL,
                                           IRBuilder<> &IRB, Value *Mem,
                                           Type *TargetIdxTy, Type *CompType,
                                           Value **Offset);

extern Type *GetPenultimateGepIndexType(Value *V);

extern int GetRemainingCompositeSizeFromElement(const DataLayout &DL,
                                                User *Val);
extern bool IsPrimitiveArrayType(Type *Ty);

extern Value *GetOffsetFromRootValue(LLVMContext &Cxt, Value *V);

extern Value *GetGEPPtrOperand(Value *V);

extern unsigned GetGEPNumIndices(Value *V);

extern Instruction *GetOperatorInstr(Value *V);
} // namespace llvm

#endif
