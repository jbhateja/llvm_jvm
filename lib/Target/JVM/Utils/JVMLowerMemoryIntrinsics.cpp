//===------------- JVMLowerMemoryIntrinsics.cpp----------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
// This file contains lowering of memory intrinsic instructions.
// Memory intrinsics (llvm.memcpy, llvm.memmove and llvm.memset) are expanded
// to assginments from source to destination.
// Size of memmory operations can be constant / variable.
//===----------------------------------------------------------------------===//

#include "JVMLowerMemoryIntrinsics.h"
#include "llvm/CodeGen/JVMCommonUtils.h"
#include "llvm/IR/ConstantFolder.h"
#include "llvm/IR/Constants.h"
#include "llvm/Transforms/Utils/LowerMemIntrinsics.h"

#define DEBUG_TYPE "jvm-lower-mem-intrinsics"

using namespace llvm;

namespace llvm {

void JVMLowerMemoryIntrinsics::AddToLazyDeletionSet(User *Elm) {
  if (DeletionSet.find(Elm) == DeletionSet.end())
    DeletionSet.insert(Elm);
}

void JVMLowerMemoryIntrinsics::PopulateEraseUsers(User *I,
                                                  SetVector<User *> &Users) {
  for (auto *User : I->users()) {
    if (User->getNumUses())
      PopulateEraseUsers(User, Users);
    Users.insert(User);
  }
}

void JVMLowerMemoryIntrinsics::ProcessLazyDeletionSet() {
  SetVector<User *> Users;
  for (auto *Val : DeletionSet) {
    PopulateEraseUsers(Val, Users);
    Users.insert(Val);
  }
  for (auto *I : Users)
    cast<Instruction>(I)->eraseFromParent();
  DeletionSet.clear();
}

Type *JVMLowerMemoryIntrinsics::GetTypeAtOffset(Type *Ty, Value **Offset) {
  if (!isa<ConstantInt>(*Offset))
    return Ty;

  Ty = GetRootType(Ty);
  const DataLayout &DL = Func->getParent()->getDataLayout();
  uint64_t OffsetVal = cast<ConstantInt>(*Offset)->getZExtValue();

  if (isa<StructType>(Ty)) {
    const StructLayout *SL = DL.getStructLayout(cast<StructType>(Ty));
    unsigned Field = SL->getElementContainingOffset(OffsetVal);
    uint64_t RemOffset = OffsetVal - SL->getElementOffset(Field);

    *Offset = ConstantInt::get(TargetIdxTy, RemOffset);
    return GetTypeAtOffset(cast<StructType>(Ty)->getElementType(Field), Offset);
  } else if (isa<ArrayType>(Ty)) {
    Type *ElmTy = cast<ArrayType>(Ty)->getElementType();
    uint64_t ElmSz = DL.getTypeSizeInBits(ElmTy) / 8;
    unsigned Index = OffsetVal / ElmSz;
    uint64_t RemOffset =
        (OffsetVal > Index * ElmSz) ? OffsetVal - ElmSz * Index : 0;

    *Offset = ConstantInt::get(TargetIdxTy, RemOffset);
    return GetTypeAtOffset(ElmTy, Offset);
  } else {
    return Ty;
  }
}

bool JVMLowerMemoryIntrinsics::CheckForPartialElementAccess(Type *Ty,
                                                            unsigned Offset) {
  const DataLayout &DL = Func->getParent()->getDataLayout();
  if (isa<ArrayType>(Ty)) {
    Type *ElemTy = cast<ArrayType>(Ty)->getElementType();
    unsigned ElmSz = DL.getTypeSizeInBits(ElemTy) / 8;
    if (isa<CompositeType>(ElemTy)) {
      Offset = Offset % ElmSz;
      return CheckForPartialElementAccess(ElemTy, Offset);
    } else
      return 0 != (Offset % ElmSz);
  } else if (isa<StructType>(Ty)) {
    const StructLayout *SL = DL.getStructLayout(cast<StructType>(Ty));
    unsigned FldIdx = SL->getElementContainingOffset(Offset);
    Type *FldTy = cast<StructType>(Ty)->getTypeAtIndex(FldIdx);
    if (isa<CompositeType>(FldTy)) {
      Offset -= SL->getElementOffset(FldIdx);
      return CheckForPartialElementAccess(FldTy, Offset);
    } else
      return SL->getElementOffset(FldIdx) == Offset;
  }
  return 0 != Offset;
}

bool JVMLowerMemoryIntrinsics::IsPartialElementAccess(Value *V) {
  bool HasBitCastInChain = false;
  Value *Offset = ConstantInt::get(TargetIdxTy, 0);
  V = GetRootObject(V, &Offset, HasBitCastInChain);

  // For element at variable offset we conservatively assume
  // a partial element access.
  if (!isa<ConstantInt>(Offset))
    return true;

  Type *Ty = GetRootType(V->getType());
  unsigned OffsetVal = cast<ConstantInt>(Offset)->getZExtValue();
  return CheckForPartialElementAccess(Ty, OffsetVal);
}

void JVMLowerMemoryIntrinsics::OptMemTransferArray(IRBuilder<> &IRB, Value *Src,
                                                   Value *SrcOffset, Value *Dst,
                                                   Value *DstOffset,
                                                   Value *Length,
                                                   MemIntrinsic *Inst) {
  auto IsPrimArray = [](Value *V) {
    Type *Ty = GetPenultimateGepIndexType(V);
    return IsPrimitiveArrayType(Ty);
  };
  const int TripCountThreshold = 5;
  bool IsMemSet = isa<MemSetInst>(Inst);
  LLVMContext &Cxt = Func->getContext();
  Type *Int8Ty = Type::getInt8Ty(Cxt);
  const DataLayout &DL = Func->getParent()->getDataLayout();
  TargetTransformInfo TTI(DL);
  Type *SrcTy = GetRootType(Src->getType());
  Type *DstTy = GetRootType(Dst->getType());
  bool IsPrimSrcAndDestArray = (IsMemSet | IsPrimArray(Src)) & IsPrimArray(Dst);
  unsigned SrcOrDstOffsets = cast<ConstantInt>(SrcOffset)->getZExtValue() |
                             cast<ConstantInt>(DstOffset)->getZExtValue();

  if (IsPrimSrcAndDestArray && (SrcTy == DstTy || IsMemSet)) {
    bool PartialMemAccess = false;
    if (0 == SrcOrDstOffsets) {
      Type *ElemTy = isa<ArrayType>(DstTy)
                         ? cast<ArrayType>(DstTy)->getElementType()
                         : DstTy;
      unsigned ElmSz = DL.getTypeSizeInBits(ElemTy) / 8;
      unsigned TripCount = cast<ConstantInt>(Length)->getZExtValue() / ElmSz;
      PartialMemAccess =
          cast<ConstantInt>(Length)->getZExtValue() % ElmSz ? true : false;
      if (false == PartialMemAccess) {
        // Generate a memcpy loop if tripcount is greater than threshold value
        // else do a element wise copy.
        if (TripCount > TripCountThreshold) {
          if (MemSetInst *MSI = dyn_cast<MemSetInst>(Inst)) {
            Value *SetValue =
                PrepareValueForMemSet(IRB, MSI->getValue(), DstTy);
            llvm::expandMemSetAsLoopUnwrapped(
                Inst, Dst,
                cast<ConstantInt>(ConstantInt::get(TargetIdxTy, TripCount)),
                SetValue, MSI->getDestAlignment(), false);
          } else {
            llvm::createMemCpyLoopKnownSizeUnwrapped(
                &*IRB.GetInsertPoint(), Src, Dst, cast<ConstantInt>(Length), 0,
                0, false, false, ElemTy, TripCount, TTI);
          }
        } else {
          for (unsigned i = 0; i < TripCount; i++) {
            Value *LoadSrc = nullptr;
            Value *DstIdx = CreateGEPIncLastIndex(IRB, TargetIdxTy, Dst, i);
            if (MemSetInst *MSI = dyn_cast<MemSetInst>(Inst)) {
              Value *SetValue = MSI->getValue();
              LoadSrc = PrepareValueForMemSet(IRB, SetValue, DstTy);
            } else {
              Value *SrcIdx = CreateGEPIncLastIndex(IRB, TargetIdxTy, Src, i);
              LoadSrc = IRB.CreateLoad(SrcIdx);
            }
            IRB.CreateStore(LoadSrc, DstIdx);
          }
        }
      }
    }
    if (SrcOrDstOffsets || PartialMemAccess) {
      // TODO: Peel off starting and end non-element aligned memory fragments
      // and rest of part is copied either as memcopy loop or element wise
      // copy.
      OptMemTransferSerial(IRB, Src, SrcOffset, Dst, DstOffset, Length, Inst);
    }
  } else if (!IsPrimSrcAndDestArray)
    OptMemTransferSerial(IRB, Src, SrcOffset, Dst, DstOffset, Length, Inst);
  else {
    // Fundamental types of src and destination elements do not match.
    // Create byte level copy loop.
    llvm::createMemCpyLoopKnownSizeUnwrapped(
        &*IRB.GetInsertPoint(), Src, Dst, cast<ConstantInt>(Length), 0, 0,
        false, false, Int8Ty, cast<ConstantInt>(Length)->getZExtValue(), TTI);
  }
}

void JVMLowerMemoryIntrinsics::OptimizeMemCopy(IRBuilder<> &IRB,
                                               MemIntrinsic *Inst) {
  bool HasBitCastInChain = false;
  Value *Src = GetMemorySrc(Inst);
  Value *Dst = Inst->getRawDest();
  Value *Length = Inst->getLength();
  const DataLayout &DL = Func->getParent()->getDataLayout();

  Value *InitOffsetSrc = ConstantInt::get(TargetIdxTy, 0);
  Src = GetRootObject(Src, &InitOffsetSrc, HasBitCastInChain);
  Src = CreateCompositeElemAccessGEP(
      DL, IRB, Src, TargetIdxTy, GetRootType(Src->getType()), &InitOffsetSrc);
  Type *PenultimateSrcTy = GetPenultimateGepIndexType(Src);

  Value *InitOffsetDst = ConstantInt::get(TargetIdxTy, 0);
  Dst = GetRootObject(Dst, &InitOffsetDst, HasBitCastInChain);
  Dst = CreateCompositeElemAccessGEP(
      DL, IRB, Dst, TargetIdxTy, GetRootType(Dst->getType()), &InitOffsetDst);
  Type *PenultimateDstTy = GetPenultimateGepIndexType(Dst);

  if (isa<ArrayType>(PenultimateDstTy) &&
      (isa<ArrayType>(PenultimateSrcTy) || isa<MemSetInst>(Inst)))
    OptMemTransferArray(IRB, Src, InitOffsetSrc, Dst, InitOffsetDst, Length,
                        Inst);
  else
    OptMemTransferSerial(IRB, Src, InitOffsetSrc, Dst, InitOffsetDst, Length,
                         Inst);
}

void JVMLowerMemoryIntrinsics::ProcessIntrinsics(
    SmallVectorImpl<MemIntrinsic *> &WorkList) {
  const DataLayout &DL = Func->getParent()->getDataLayout();
  TargetTransformInfo TTI(DL);
  while (!WorkList.empty()) {
    auto Item = WorkList.back();
    WorkList.pop_back();
    IRBuilder<> IRB(Item);
    if (isa<ConstantInt>(Item->getLength()))
      OptimizeMemCopy(IRB, Item);
    else if (MemCpyInst *Memcpy = dyn_cast<MemCpyInst>(Item))
      expandMemCpyAsLoop(Memcpy, TTI);
    else if (MemSetInst *Memset = dyn_cast<MemSetInst>(Item))
      expandMemSetAsLoop(Memset);
    ResetOffsets();
    AddToLazyDeletionSet(cast<User>(Item));
  }
}

Value *JVMLowerMemoryIntrinsics::TransformMemMoveToCpy(Value *I) {
  MemTransferInst *MI = cast<MemTransferInst>(I);
  IRBuilder<> IRB(MI);
  Value *NewI = IRB.CreateMemCpy(MI->getRawDest(), MI->getDestAlignment(),
                                 MI->getRawSource(), MI->getSourceAlignment(),
                                 MI->getLength());
  I->replaceAllUsesWith(NewI);
  cast<Instruction>(I)->dropAllReferences();
  AddToLazyDeletionSet(cast<User>(I));

  return NewI;
}

bool JVMLowerMemoryIntrinsics::ProcessFunction() {
  LLVMContext &Cxt = Func->getContext();
  SmallVector<MemIntrinsic *, 4> WorkList;
  const DataLayout &DL = Func->getParent()->getDataLayout();
  TargetIdxTy = GetTargetIndexType(DL, Cxt);

  for (auto &BB : *Func)
    for (auto &I : BB)
      if (isa<MemIntrinsic>(&I)) {
        Value *MemI = isa<MemMoveInst>(&I) ? TransformMemMoveToCpy(&I) : &I;
        WorkList.push_back(cast<MemIntrinsic>(MemI));
      }

  ProcessIntrinsics(WorkList);
  ProcessLazyDeletionSet();
  return true;
}

Value *JVMLowerMemoryIntrinsics::PrepareValueForMemSet(IRBuilder<> &IRB,
                                                       Value *Val,
                                                       Type *ToType) {
  SmallVector<Constant *, 4> Elems;
  if (StructType *STy = dyn_cast<StructType>(ToType)) {
    for (unsigned i = 0; i < STy->getNumElements(); i++) {
      Type *Ty = STy->getTypeAtIndex(i);
      Elems.push_back(cast<Constant>(PrepareValueForMemSet(IRB, Val, Ty)));
    }
    return ConstantStruct::get(STy, Elems);
  } else if (ArrayType *ATy = dyn_cast<ArrayType>(ToType)) {
    Type *Ty = ATy->getElementType();
    Value *ElmVal = PrepareValueForMemSet(IRB, Val, Ty);
    for (unsigned i = 0; i < ATy->getNumElements(); i++)
      Elems.push_back(cast<Constant>(ElmVal));
    return ConstantArray::get(ATy, Elems);
  } else
    return PrepareValueForMemScalar(IRB, Val, ToType);
}

Value *JVMLowerMemoryIntrinsics::PrepareValueForMemScalar(IRBuilder<> &IRB,
                                                          Value *Val,
                                                          Type *ToType) {
  assert(isa<Constant>(Val));
  const DataLayout &DL = Func->getParent()->getDataLayout();
  assert(DL.getTypeSizeInBits(ToType) >= DL.getTypeSizeInBits(Val->getType()));
  unsigned PackCount =
      DL.getTypeSizeInBits(ToType) / DL.getTypeSizeInBits(Val->getType());
  Value *Ret = ConstantInt::get(ToType, cast<ConstantInt>(Val)->getZExtValue());
  for (unsigned i = 1; i < PackCount; i++) {
    Value *Shift = ConstantInt::get(Ret->getType(),
                                    DL.getTypeSizeInBits(Val->getType()) * i);
    Value *ShiftVal = IRB.CreateShl(Ret, Shift);
    Ret = IRB.CreateOr(Ret, ShiftVal);
  }
  return Ret;
}

void JVMLowerMemoryIntrinsics::SetInitGEPs(Value *Src, Value *SrcOffset,
                                           Value *Dst, Value *DstOffset) {
  // For non-constant offsets memcpy loop needs to be generated for copy
  // involving arrays and for structures we giveup and mark it as
  // non-transalatble during candidate selection. Candidate checker does this
  // checks currently.
  assert(isa<ConstantInt>(SrcOffset) && "Non-constant offsets encountered");
  assert(isa<ConstantInt>(DstOffset) && "Non-constant offsets encountered");
  CurrSrc = Src;
  CurrSrcOffset = cast<ConstantInt>(SrcOffset)->getZExtValue();
  CurrDst = Dst;
  CurrDstOffset = cast<ConstantInt>(DstOffset)->getZExtValue();
}

Value *JVMLowerMemoryIntrinsics::GetNextGep(IRBuilder<> &IRB, Value *V,
                                            bool IsSrcGep) {
  unsigned Padding = 0;
  assert(isa<GetElementPtrInst>(V) &&
         "GEP argument expected for NextGEP computation");

  const DataLayout &DL = Func->getParent()->getDataLayout();
  Value *NextGep = llvm::GetNextGEP(DL, IRB, TargetIdxTy, V, Padding);

  // Adjust Src/Dst offsets to accomodate field padding for
  // structure types.
  if (IsSrcGep)
    CurrDstOffset += Padding;
  else
    CurrSrcOffset += Padding;

  return NextGep;
}

Value *JVMLowerMemoryIntrinsics::GetNextDstGep(IRBuilder<> &IRB) {
  const DataLayout &DL = Func->getParent()->getDataLayout();
  unsigned DstSz = DL.getTypeSizeInBits(GetRootType(CurrDst->getType()));
  if (CurrDstOffset < DstSz)
    return CurrDst;

  CurrDstOffset = 0;
  return GetNextGep(IRB, CurrDst, true);
}

Value *JVMLowerMemoryIntrinsics::GetNextSrcGep(IRBuilder<> &IRB) {
  const DataLayout &DL = Func->getParent()->getDataLayout();
  unsigned SrcSz = DL.getTypeSizeInBits(GetRootType(CurrSrc->getType()));
  if (CurrSrcOffset < SrcSz)
    return CurrSrc;

  CurrSrcOffset = 0;
  return GetNextGep(IRB, CurrSrc, false);
}

Value *JVMLowerMemoryIntrinsics::GetDstMask(unsigned WindowSz) {
  const DataLayout &DL = Func->getParent()->getDataLayout();
  unsigned DstSz = DL.getTypeSizeInBits(CurrDst->getType());
  APInt Mask(DstSz, 0);
  if (CurrDstOffset)
    Mask.setBits(0, CurrDstOffset);
  Mask.setBits(CurrDstOffset + WindowSz, DstSz);
  return ConstantInt::get(TargetIdxTy, Mask.getZExtValue());
}

unsigned JVMLowerMemoryIntrinsics::OptMemTransferScalar(IRBuilder<> &IRB,
                                                        Value *Src, Value *Dst,
                                                        MemIntrinsic *Inst) {
  assert(!isa<CompositeType>(Src->getType()) &&
         !isa<CompositeType>(Dst->getType()));
  const DataLayout &DL = Func->getParent()->getDataLayout();
  Type *SrcTy = GetRootType(Src->getType());
  Type *DstTy = GetRootType(Dst->getType());
  uint64_t SrcSz = DL.getTypeSizeInBits(SrcTy);
  uint64_t DstSz = DL.getTypeSizeInBits(DstTy);
  unsigned MemTransferDst = DstSz - CurrDstOffset;
  unsigned MemTransferSrc =
      isa<MemSetInst>(Inst) ? MemTransferDst : SrcSz - CurrSrcOffset;
  unsigned MemTransferSz =
      MemTransferDst < MemTransferSrc ? MemTransferDst : MemTransferSrc;

  Value *LoadSrc = nullptr;
  if (MemSetInst *MSI = dyn_cast<MemSetInst>(Inst)) {
    LoadSrc = PrepareValueForMemSet(IRB, MSI->getValue(), DstTy);
    SrcSz = DstSz;
  } else
    LoadSrc = IRB.CreateLoad(Src);

  if (SrcSz == DstSz && 0 == CurrDstOffset && 0 == CurrSrcOffset)
    IRB.CreateStore(LoadSrc, Dst);
  else if (SrcSz > DstSz) {
    if (CurrSrcOffset)
      LoadSrc =
          IRB.CreateLShr(LoadSrc, ConstantInt::get(TargetIdxTy, CurrSrcOffset));
    Value *TruncSrc = IRB.CreateTrunc(LoadSrc, DstTy);
    if (CurrDstOffset) {
      Value *ShiftedTruncSrc =
          IRB.CreateShl(TruncSrc, ConstantInt::get(TargetIdxTy, CurrDstOffset));
      Value *LoadDst = IRB.CreateLoad(Dst);
      Value *DstMask = GetDstMask(MemTransferSz);
      Value *MaskedDst = IRB.CreateAnd(LoadDst, DstMask);
      TruncSrc = IRB.CreateOr(ShiftedTruncSrc, MaskedDst);
    }
    IRB.CreateStore(TruncSrc, Dst);
  } else {
    if (CurrSrcOffset)
      LoadSrc =
          IRB.CreateLShr(LoadSrc, ConstantInt::get(TargetIdxTy, CurrSrcOffset));
    Value *WidenedLoadVal = IRB.CreateZExt(LoadSrc, DstTy);
    Value *ShiftedVal = IRB.CreateShl(WidenedLoadVal, CurrDstOffset);
    Value *DstMask = GetDstMask(MemTransferSz);
    Value *LoadDst = IRB.CreateLoad(Dst);
    Value *MaskedDst = IRB.CreateAnd(LoadDst, DstMask);
    Value *StoreVal = IRB.CreateOr(ShiftedVal, MaskedDst);
    IRB.CreateStore(StoreVal, Dst);
  }
  CurrDstOffset += MemTransferSz;
  CurrSrcOffset += isa<MemSetInst>(Inst) ? 0 : MemTransferSz;
  return MemTransferSz;
}

void JVMLowerMemoryIntrinsics::OptMemTransferSerial(
    IRBuilder<> &IRB, Value *Src, Value *SrcOffset, Value *Dst,
    Value *DstOffset, Value *Length, MemIntrinsic *Inst) {
  SetInitGEPs(Src, SrcOffset, Dst, DstOffset);
  int MemTransferSz = (int)cast<ConstantInt>(Length)->getZExtValue() * 8;
  while (MemTransferSz > 0) {
    MemTransferSz -= (int)OptMemTransferScalar(IRB, CurrSrc, CurrDst, Inst);
    if (MemTransferSz > 0) {
      CurrSrc = GetNextSrcGep(IRB);
      CurrDst = GetNextDstGep(IRB);
    }
  }
}

Value *JVMLowerMemoryIntrinsics::GetUnwrappedValue(Value *V) {
  while (isa<CastInst>(V))
    V = cast<CastInst>(V)->getOperand(0);
  return V;
}

void JVMLowerMemoryIntrinsics::ResetOffsets() {
  CurrDstOffset = 0;
  CurrSrcOffset = 0;
  CurrSrc = nullptr;
  CurrDst = nullptr;
}
} // namespace llvm

char JVMLowerMemoryIntrinsics::ID = 0;
static RegisterPass<JVMLowerMemoryIntrinsics>
    JVMMemIntrinsics("jvm-lower-memintrinsics",
                     "JVM Lower Memory Intrinsics Pass");
namespace llvm {
FunctionPass *createJVMLowerMemoryIntrinsics() {
  return new JVMLowerMemoryIntrinsics();
}
} // namespace llvm
