//===-------------JVMCommonUtils.cpp----------------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
// This file contains utility functions usable by all the handlers.
//===----------------------------------------------------------------------===//

#include "llvm/CodeGen/JVMCommonUtils.h"
#include "llvm/IR/Constants.h"

using namespace llvm;

namespace llvm {

Type *GetRootType(Type *Ty) {
  if (isa<PointerType>(Ty))
    return cast<PointerType>(Ty)->getElementType();
  return Ty;
}

Type *GetNormalizedRootType(Type *Ty, const DataLayout &DL, LLVMContext &Cxt) {
  auto NormalizedType = [&](Type *Ty) {
    if (isa<CompositeType>(Ty) || !isa<IntegerType>(Ty))
      return Ty;

    unsigned Sz = DL.getTypeSizeInBits(Ty);
    unsigned NextPowerOf2Sz = PowerOf2Ceil(Sz);
    if (Sz != NextPowerOf2Sz)
      return cast<Type>(IntegerType::get(Cxt, NextPowerOf2Sz));
    else
      return Ty;
  };

  if (isa<PointerType>(Ty))
    return NormalizedType(cast<PointerType>(Ty)->getElementType());
  return NormalizedType(Ty);
}

Type *GetTargetIndexType(const DataLayout &DL, LLVMContext &Cxt) {
  if (DL.getIndexSize(0) == 64)
    return Type::getInt64Ty(Cxt);
  else
    return Type::getInt32Ty(Cxt);
}

Value *GetTypeNormalizedValue(const DataLayout &DL, IRBuilder<> &IRB,
                              Value *Val, Type *Ty) {
  Type *ValTy = Val->getType();
  assert(!isa<CompositeType>(ValTy));
  unsigned TySz = DL.getTypeSizeInBits(Ty);
  unsigned ValTySz = DL.getTypeSizeInBits(ValTy);

  if (ValTySz == TySz)
    return Val;
  else if (ValTySz < TySz)
    return IRB.CreateZExt(Val, Ty);
  else
    return IRB.CreateTrunc(Val, Ty);
}

Instruction *GetOperatorInstr(Value *V) {
  if (Instruction *I = dyn_cast<Instruction>(V))
    return I;
  else {
    assert(isa<ConstantExpr>(V));
    if (V->use_empty())
      return nullptr;
    return GetOperatorInstr(*V->user_begin());
  }
}

Value *ComputeOffsetForGEP(IRBuilder<> &IRB, User &I, Value *Offset) {
  Function *Func;
  if (isa<GEPOperator>(&I)) {
    Instruction *UI = GetOperatorInstr(&I);
    Func = UI->getParent()->getParent();
  } else
    Func = cast<Instruction>(&I)->getParent()->getParent();

  LLVMContext &Cxt = Func->getContext();
  const DataLayout &DL = Func->getParent()->getDataLayout();
  Type *TargetIdxTy = GetTargetIndexType(DL, Cxt);
  for (gep_type_iterator Itr = gep_type_begin(&I); Itr != gep_type_end(&I);
       Itr++) {
    Type *IdxTy = Itr.getIndexedType();
    Value *IdxVal = Itr.getOperand();

    if (StructType *STy = Itr.getStructTypeOrNull()) {
      const StructLayout *SL = DL.getStructLayout(STy);
      unsigned ElementIdx = cast<ConstantInt>(IdxVal)->getZExtValue();
      unsigned ElmOffset = SL->getElementOffset(ElementIdx);
      Offset = IRB.CreateBinOp(Instruction::Add, Offset,
                               ConstantInt::get(TargetIdxTy, ElmOffset));
      continue;
    }

    Value *TypeSz =
        ConstantInt::get(TargetIdxTy, DL.getTypeSizeInBits(IdxTy) / 8);
    IdxVal = GetTypeNormalizedValue(DL, IRB, IdxVal, TargetIdxTy);
    Value *IdxOffset = IRB.CreateBinOp(Instruction::Mul, IdxVal, TypeSz);
    Offset = IRB.CreateBinOp(Instruction::Add, Offset, IdxOffset);
  }

  return Offset;
}

Value *GetRootObject(Value *Obj, Value **Offset, bool &HasBitCastInChain,
                     bool ComputeOffset, int RecLevel,
                     Instruction *OffsetPlaceHolderInst) {
  assert(*Offset != nullptr && "Null offset passed");
  assert(Obj && "Null object passed");

  if (isa<Argument>(Obj) || isa<AllocaInst>(Obj))
    return Obj;

  if (isa<GlobalVariable>(Obj) && cast<GlobalVariable>(Obj)->isConstant())
    return Obj;

  if (isa<ConstantInt>(Obj))
    return Obj;

  if (isa<GetElementPtrInst>(Obj) || isa<GEPOperator>(Obj)) {
    bool IsGEPOperator = isa<GEPOperator>(Obj);
    if (ComputeOffset) {
      Instruction *GEP =
          IsGEPOperator
              ? cast<Instruction>(*cast<GEPOperator>(Obj)->user_begin())
              : cast<Instruction>(Obj);
      // Placeholder makes sure that newly generate instructions
      // are placed after their operands in recursive invocations.
      OffsetPlaceHolderInst =
          OffsetPlaceHolderInst ? OffsetPlaceHolderInst : GEP;
      IRBuilder<> IRB(OffsetPlaceHolderInst);
      *Offset = ComputeOffsetForGEP(IRB, *cast<User>(Obj), *Offset);
    }
    Value *PtrOperand = GetGEPPtrOperand(Obj);
    return GetRootObject(PtrOperand, Offset, HasBitCastInChain, ComputeOffset,
                         RecLevel++, OffsetPlaceHolderInst);
  }

  if (isa<LoadInst>(Obj))
    return GetRootObject(cast<LoadInst>(Obj)->getPointerOperand(), Offset,
                         HasBitCastInChain, ComputeOffset, RecLevel++,
                         OffsetPlaceHolderInst);

  if (isa<BitCastOperator>(Obj)) {
    HasBitCastInChain = RecLevel > 0;
    return GetRootObject(cast<BitCastOperator>(Obj)->getOperand(0), Offset,
                         HasBitCastInChain, ComputeOffset, RecLevel++,
                         OffsetPlaceHolderInst);
  }

  if (isa<BitCastInst>(Obj)) {
    HasBitCastInChain = RecLevel > 0;
    return GetRootObject(cast<BitCastInst>(Obj)->getOperand(0), Offset,
                         HasBitCastInChain, ComputeOffset, RecLevel++,
                         OffsetPlaceHolderInst);
  }

  if (isa<TruncInst>(Obj) || isa<ZExtInst>(Obj) || isa<SExtInst>(Obj))
    return GetRootObject(cast<User>(Obj)->getOperand(0), Offset,
                         HasBitCastInChain, ComputeOffset, RecLevel++,
                         OffsetPlaceHolderInst);

  llvm_unreachable("Unhandled object type in GetRootObject()");
  return nullptr;
}

Value *GetRootObject2(Value *Obj, LLVMContext &Cxt) {
  bool HasBitCastInChain;
  Value *Offset = ConstantInt::get(Type::getInt32Ty(Cxt), 0);
  return GetRootObject(Obj, &Offset, HasBitCastInChain, false);
}

// To check if value is a part of / has any reference to structure we can
// do following two way check :-
// bool IsValuePartOfStruct =
// CheckIfValuePartOfStructure(Val) ||
// CheckIfLeafElementIsStructureType(Val->getType());
//
bool CheckIfLeafElementIsStructureType(Type *Ty) {
  bool RetVal = false;
  Ty = GetRootType(Ty);
  if (isa<StructType>(Ty))
    return true;
  else if (isa<ArrayType>(Ty))
    RetVal |= CheckIfLeafElementIsStructureType(
        cast<ArrayType>(Ty)->getElementType());
  else if (isa<PointerType>(Ty))
    RetVal |= CheckIfLeafElementIsStructureType(
        cast<PointerType>(Ty)->getElementType());

  return RetVal;
}

bool CheckIfValuePartOfStructure(Value *V) {
  if (isa<GetElementPtrInst>(V) || isa<GEPOperator>(V)) {
    User *GEP = cast<User>(V);
    for (auto Itr = gep_type_begin(GEP); Itr != gep_type_end(GEP); Itr++)
      if (isa<StructType>(Itr.getIndexedType()))
        return true;
    Value *PtrOperand = GetGEPPtrOperand(GEP);
    return CheckIfValuePartOfStructure(PtrOperand);
  } else if (isa<CastInst>(V))
    return CheckIfValuePartOfStructure(cast<CastInst>(V)->getOperand(0));
  else {
    Type *Ty = GetRootType(V->getType());
    if (isa<StructType>(Ty))
      return true;
  }
  return false;
}

Value *GetMemorySrc(Value *MI) {
  if (isa<MemTransferInst>(MI))
    return cast<MemTransferInst>(MI)->getRawSource();
  else if (isa<MemSetInst>(MI))
    return cast<MemSetInst>(MI)->getValue();
  else
    llvm_unreachable("Unhandled memory intrinsic");
}

Value *GenMask(LLVMContext &Cxt, const DataLayout &DL, IRBuilder<> &IRB,
               unsigned SrcSz, unsigned DestSz, Value *Offset, bool IsLoad) {
  assert(SrcSz && DestSz && "Empty source/destination");
  Value *MaskVal = nullptr;
  APInt Mask(SrcSz, 0);
  if (isa<ConstantInt>(Offset)) {
    uint64_t ByteOffset = cast<ConstantInt>(Offset)->getZExtValue();
    assert((SrcSz >= (DestSz + ByteOffset * 8)) && "Overflow detected");

    uint64_t Shift = ByteOffset * 8;
    if (IsLoad)
      Mask.setBits(Shift, Shift + DestSz);
    else {
      Mask.setBits(0, Shift);
      Mask.setBits(Shift + DestSz, SrcSz);
    }

    MaskVal = ConstantInt::get(Cxt, Mask);
  } else {
    MaskVal = ConstantInt::get(Cxt, Mask);
    Type *CoercedTy = MaskVal->getType();
    Offset = GetTypeNormalizedValue(DL, IRB, Offset, CoercedTy);
    if (IsLoad) {
      Mask.setBits(0, DestSz);
      Value *Shift = IRB.CreateBinOp(Instruction::Mul, Offset,
                                     ConstantInt::get(CoercedTy, 8));
      MaskVal = IRB.CreateBinOp(Instruction::Shl, MaskVal, Shift);
    } else {
      Mask.setBits(0, DestSz);
      MaskVal = ConstantInt::get(Cxt, Mask);
      Value *Shift = IRB.CreateBinOp(Instruction::Mul, Offset,
                                     ConstantInt::get(CoercedTy, 8));
      Value *ShiftedMask = IRB.CreateBinOp(Instruction::Shl, MaskVal, Shift);

      Mask.setBits(0, SrcSz);
      MaskVal = ConstantInt::get(Cxt, Mask);
      MaskVal = IRB.CreateBinOp(Instruction::Xor, ShiftedMask, MaskVal);
    }
  }
  return MaskVal;
}

Value *GetGEPPtrOperand(Value *V) {
  assert(isa<GetElementPtrInst>(V) || isa<GEPOperator>(V));
  if (isa<GEPOperator>(V))
    return cast<GEPOperator>(V)->getPointerOperand();
  else
    return cast<GetElementPtrInst>(V)->getPointerOperand();
}

unsigned GetGEPNumIndices(Value *V) {
  assert(isa<GetElementPtrInst>(V) || isa<GEPOperator>(V));
  if (isa<GEPOperator>(V))
    return cast<GEPOperator>(V)->getNumIndices();
  else
    return cast<GetElementPtrInst>(V)->getNumIndices();
}

Value *CreateGEPIncLastIndex(IRBuilder<> &IRB, Type *TargetIdxTy, Value *I,
                             unsigned i) {
  assert(isa<GetElementPtrInst>(I) || isa<GEPOperator>(I));
  if (0 == i)
    return I;

  unsigned IdxCnt = 0;
  Value *LastIndex = nullptr;
  SmallVector<Value *, 8> Indices;
  bool IsGEPOperator = isa<GEPOperator>(I);
  auto ItrBegin = IsGEPOperator ? cast<GEPOperator>(I)->idx_begin()
                                : cast<GetElementPtrInst>(I)->idx_begin();
  auto ItrEnd = IsGEPOperator ? cast<GEPOperator>(I)->idx_end()
                              : cast<GetElementPtrInst>(I)->idx_end();
  unsigned NumIndices = GetGEPNumIndices(I);
  Value *PtrOperand = GetGEPPtrOperand(I);

  while (ItrBegin != ItrEnd) {
    if (IdxCnt++ == (NumIndices - 1)) {
      LastIndex = *ItrBegin;
      break;
    }
    Indices.push_back(*ItrBegin);
    ItrBegin++;
  }
  Value *NewIndex = IRB.CreateAdd(LastIndex, ConstantInt::get(TargetIdxTy, i));
  Indices.push_back(NewIndex);
  return IRB.CreateGEP(PtrOperand, Indices);
}

Value *GetNextGEP(const DataLayout &DL, IRBuilder<> &IRB, Type *TargetIdxTy,
                  Value *V, unsigned &Padding) {
  using TypeIndexPair = std::pair<Type *, Value *>;
  using TypeIndexPairVec = SmallVector<TypeIndexPair, 8>;

  auto UpdatePadding = [&](Type *Ty, unsigned PrevField) {
    if (!isa<StructType>(Ty))
      return;
    StructType *STy = cast<StructType>(Ty);
    const StructLayout *SL = DL.getStructLayout(STy);
    if (SL->hasPadding()) {
      Padding = ((SL->getElementOffset(PrevField + 1) * 8) -
                 ((SL->getElementOffset(PrevField) * 8) +
                  DL.getTypeSizeInBits(STy->getTypeAtIndex(PrevField))));
    }
  };

  Value *NextGEP = nullptr;
  TypeIndexPairVec WorkList;
  User *GEP = cast<User>(V);
  bool SrcMemPartOfStruct = CheckIfValuePartOfStructure(GEP);
  for (gep_type_iterator Itr = gep_type_begin(GEP); Itr != gep_type_end(GEP);
       Itr++)
    WorkList.push_back(std::make_pair(Itr.getIndexedType(), Itr.getOperand()));

  Value *PrevIndex = nullptr;
  Type *NewLeafType = nullptr;
  Value *NewLeafIndex = nullptr;
  while (!WorkList.empty()) {
    TypeIndexPair TyIdxPair = WorkList.back();
    if (isa<ArrayType>(TyIdxPair.first) || isa<StructType>(TyIdxPair.first)) {
      Type *Ty = TyIdxPair.first;

      // With variable index we cannot deterministically determine when we
      // exhausted array and should move to next field.
      // This is a problem with array fields of a structure, for standalone
      // arrays overrun shall be dealt by an exception thrown at run time
      // by JVM.
      // Currently CandidateChecker invalidates the function as translation
      // candidate if source memory of structure types are being accessed
      // by a variable offset (computed by bitcasted memory access pattern).
      assert((isa<ArrayType>(Ty) &&
              (!SrcMemPartOfStruct || isa<ConstantInt>(PrevIndex))) ||
             (isa<StructType>(Ty) && isa<ConstantInt>(PrevIndex)));
      unsigned NumElems = isa<ArrayType>(Ty)
                              ? cast<ArrayType>(Ty)->getNumElements()
                              : cast<StructType>(Ty)->getNumElements();
      if (isa<ConstantInt>(PrevIndex)) {
        unsigned PrevElemIdx = cast<ConstantInt>(PrevIndex)->getZExtValue();
        unsigned NextElemIdx = PrevElemIdx + 1;
        // For homogeneous type like array overrun is ok as it will be dealt
        // by run-time reference checking done by JVM but for heterogeneous type
        // like structure overrun cannot be done since type of overrun field
        // cannot be determined.
        bool AllowOutOfBoundsAccesss =
            isa<ArrayType>(Ty) && !SrcMemPartOfStruct;
        if (NextElemIdx < NumElems || AllowOutOfBoundsAccesss) {
          NewLeafIndex = ConstantInt::get(TargetIdxTy, NextElemIdx);
          NewLeafType = cast<CompositeType>(Ty)->getTypeAtIndex(NewLeafIndex);
          UpdatePadding(Ty, PrevElemIdx);
          break;
        }
      } else {
        NewLeafIndex =
            IRB.CreateAdd(PrevIndex, ConstantInt::get(TargetIdxTy, 1));
        NewLeafType = cast<CompositeType>(Ty)->getTypeAtIndex(NewLeafIndex);
        break;
      }
    }
    WorkList.pop_back();
    PrevIndex = TyIdxPair.second;
  }

  if (NewLeafIndex) {
    SmallVector<Value *, 4> Indices;
    for (unsigned i = 0; i < WorkList.size(); i++)
      Indices.push_back(WorkList[i].second);
    Indices.push_back(NewLeafIndex);
    if (isa<CompositeType>(NewLeafType) && isa<ConstantInt>(NewLeafIndex)) {
      Value *Offset = ConstantInt::get(TargetIdxTy, 0);
      GetIndicesForCompositeElemAccess(DL, IRB, TargetIdxTy, Indices,
                                       NewLeafType, &Offset);
    }
    Value *PtrOperand = GetGEPPtrOperand(GEP);
    NextGEP = IRB.CreateGEP(PtrOperand, Indices);
  }

  return NextGEP;
}

unsigned GetStructElmIndexAtOffset(const DataLayout &DL, StructType *STy,
                                   unsigned Offset, unsigned &PadOffset) {
  unsigned i = 0;
  const StructLayout *SL = DL.getStructLayout(STy);
  unsigned ElmIdx = 0;
  for (; i < STy->getNumElements(); i++) {
    ElmIdx = i;
    unsigned ElmOffset = SL->getElementOffset(i);
    if (ElmOffset >= Offset) {
      ElmIdx = ElmOffset > Offset ? i - 1 : i;
      break;
    }
  }

  unsigned ElmSz = DL.getTypeSizeInBits(STy->getTypeAtIndex(ElmIdx)) / 8;
  if (SL->hasPadding() && (Offset > (SL->getElementOffset(ElmIdx) + ElmSz))) {
    PadOffset = ElmSz + SL->getElementOffset(ElmIdx);
  }
  return ElmIdx;
}

void GetIndicesForCompositeElemAccess(const DataLayout &DL, IRBuilder<> &IRB,
                                      Type *TargetIdxTy,
                                      SmallVectorImpl<Value *> &Indices,
                                      Type *CompType, Value **Offset) {
  if (isa<StructType>(CompType)) {
    assert(isa<ConstantInt>(*Offset) &&
           "Variable offset used for field access");
    unsigned PadOffset = 0;
    unsigned ConstOffset = cast<ConstantInt>(*Offset)->getZExtValue();
    StructType *STy = cast<StructType>(CompType);
    const StructLayout *SL = DL.getStructLayout(STy);
    int i = GetStructElmIndexAtOffset(DL, STy, ConstOffset, PadOffset);
    Indices.push_back(ConstantInt::get(TargetIdxTy, i));
    unsigned ElmOffset = SL->getElementOffset(i);
    Value *ElmOffsetVal = ConstantInt::get(TargetIdxTy, ElmOffset);
    *Offset = IRB.CreateBinOp(Instruction::Sub, *Offset, ElmOffsetVal);
    if (isa<CompositeType>(STy->getTypeAtIndex(i)))
      GetIndicesForCompositeElemAccess(DL, IRB, TargetIdxTy, Indices,
                                       STy->getTypeAtIndex(i), Offset);
  } else if (isa<ArrayType>(CompType)) {
    Type *ElmTy = cast<ArrayType>(CompType)->getElementType();
    Value *ElmSz =
        ConstantInt::get(TargetIdxTy, DL.getTypeSizeInBits(ElmTy) / 8);
    Value *Index = IRB.CreateBinOp(Instruction::UDiv, *Offset, ElmSz);
    Indices.push_back(Index);
    *Offset = IRB.CreateBinOp(Instruction::URem, *Offset, ElmSz);
    if (isa<CompositeType>(ElmTy))
      GetIndicesForCompositeElemAccess(DL, IRB, TargetIdxTy, Indices, ElmTy,
                                       Offset);
  } else {
    Value *CompTypeSz =
        ConstantInt::get(TargetIdxTy, DL.getTypeSizeInBits(CompType) / 8);
    Value *Index = IRB.CreateBinOp(Instruction::UDiv, *Offset, CompTypeSz);
    Indices.push_back(Index);
    *Offset = IRB.CreateBinOp(Instruction::Sub, *Offset, CompTypeSz);
  }
}

Value *CreateCompositeElemAccessGEP(const DataLayout &DL, IRBuilder<> &IRB,
                                    Value *Mem, Type *TargetIdxTy,
                                    Type *CompType, Value **Offset) {
  if (!isa<CompositeType>(CompType))
    return Mem;
  SmallVector<Value *, 4> Indices;
  Indices.push_back(ConstantInt::get(TargetIdxTy, 0));
  GetIndicesForCompositeElemAccess(DL, IRB, TargetIdxTy, Indices, CompType,
                                   Offset);
  return IRB.CreateGEP(Mem, Indices);
}

Type *GetPenultimateGepIndexType(Value *V) {
  if (!isa<GetElementPtrInst>(V) && !isa<GEPOperator>(V))
    return GetRootType(V->getType());

  Value *PtrOperand = GetGEPPtrOperand(V);
  assert(isa<CompositeType>(GetRootType(PtrOperand->getType())));
  unsigned NumIndices = GetGEPNumIndices(V);

  auto Itr = gep_type_begin(cast<User>(V));
  while (NumIndices > 2) {
    NumIndices--;
    Itr++;
  }
  return GetRootType(Itr.getIndexedType());
}

int GetRemainingCompositeSizeFromElement(const DataLayout &DL, User *Val) {
  assert(isa<GetElementPtrInst>(Val));
  Value *LastIndex = Val->getOperand(GetGEPNumIndices(Val));
  if (!cast<ConstantInt>(LastIndex))
    return -1;

  unsigned LastIndexVal = cast<ConstantInt>(LastIndex)->getZExtValue();
  Type *Ty = GetPenultimateGepIndexType(Val);
  assert(isa<CompositeType>(Ty));

  unsigned CompSz = DL.getTypeSizeInBits(Ty);
  if (isa<ArrayType>(Ty)) {
    Type *ElmTy = cast<ArrayType>(Ty)->getElementType();
    unsigned ElmSz = DL.getTypeSizeInBits(ElmTy);
    unsigned CurrElmSz = (LastIndexVal + 1) * ElmSz;
    return CompSz - CurrElmSz;
  } else if (isa<StructType>(Ty)) {
    const StructLayout *SL = DL.getStructLayout(cast<StructType>(Ty));
    unsigned CurrElmSz = SL->getElementOffset(LastIndexVal + 1);
    return CompSz - CurrElmSz;
  } else
    llvm_unreachable("Unexpected composite type");

  return -1;
}

Value *GetOffsetFromRootValue(LLVMContext &Cxt, Value *V) {
  bool HasBitCastInChain;
  Value *Offset = ConstantInt::get(Type::getInt32Ty(Cxt), 0);
  GetRootObject(V, &Offset, HasBitCastInChain);
  return Offset;
}

bool IsPrimitiveArrayType(Type *Ty) {
  if (isa<ArrayType>(Ty))
    return IsPrimitiveArrayType(cast<ArrayType>(Ty)->getElementType());
  else if (isa<CompositeType>(Ty))
    return false;
  return true;
}
} // namespace llvm
