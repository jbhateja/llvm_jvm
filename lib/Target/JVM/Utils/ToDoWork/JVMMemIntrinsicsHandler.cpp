//===------------- JVMMemIntrinsicsHandler.cpp---------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
// This file contains handling for memory intrinsic instructions.
// Memory intrinsics (llvm.memcpy, llvm.memmove and llvm.memset) are expanded
// to assginments from source to destination. 
// Size of memmory operations can be constant / variable. 
//===----------------------------------------------------------------------===//

#include "JVMMemIntrinsicsHandler.h"
#include "JVMCommonUtils.h"
#include "llvm/IR/ConstantFolder.h"

#define DEBUG_TYPE "mem-intrinsics-handler"

using namespace llvm;

namespace llvm {

bool MemIntrinsicsHandler::ProcessUser(SrcMemOpOffset &MemOpOffsetMap, User &I,
                              Value *Offset, bool isStruct) {
  bool RetVal = true;
  if (isa<GetElementPtrInst>(&I))
    RetVal &= ProcessGEPChain(MemOpOffsetMap, *cast<GetElementPtrInst>(&I),
                              Offset, isStruct);
  else if (isa<LoadInst>(&I) || isa<StoreInst>(&I))
    MemOpOffsetMap[&I] = Offset;
  else if (isa<BitCastInst>(&I)) {
    AddToLazyDeletionSet(&I);
    for (auto *BCUser : cast<BitCastInst>(&I)->users())
      RetVal &= ProcessUser(MemOpOffsetMap, *BCUser, Offset, isStruct);
  } else
    llvm_unreachable("Unhandled insturction in ProcessGEPUser");

  return RetVal;
}

bool MemIntrinsicsHandler::ProcessGEPChain(SrcMemOpOffset &MemOpOffsetMap,
                                  GetElementPtrInst &I, Value *Offset,
                                  bool isStruct) {
  IRBuilder<> IRB(&I);
  bool RetVal = true;
  bool AllConstantIndices = I.hasAllConstantIndices();
  if (isStruct && !AllConstantIndices)
    return false;

  Offset = ComputeOffsetForGEP(IRB, I, Offset);

  AddToLazyDeletionSet(&I);
  for (auto *User : I.users())
    RetVal &= ProcessUser(MemOpOffsetMap, *User, Offset, isStruct);

  return RetVal;
}

Value *MemIntrinsicsHandler::GenMask(IRBuilder<> &IRB, unsigned SrcSz, unsigned DestSz,
                            Value *Offset, bool IsLoad) {
  assert(SrcSz && DestSz && "Empty source/destination");
  Value *MaskVal = nullptr;
  LLVMContext &Cxt = Func->getContext();
  APInt Mask(SrcSz, 0);
  if (isa<ConstantInt>(Offset)) {
    uint64_t ByteOffset = cast<ConstantInt>(Offset)->getZExtValue();
    assert((SrcSz >= (DestSz + ByteOffset * 8)) && "Overflow detected");

    uint64_t Shift = ByteOffset * 8;
    if (IsLoad)
      Mask.setBits(Shift, Shift + DestSz - 1);
    else {
      Mask.setBits(0, Shift);
      Mask.setBits(Shift + DestSz, SrcSz);
    }

    MaskVal = ConstantInt::get(Cxt, Mask);
  } else {
    if (IsLoad) {
      Mask.setBits(0, DestSz - 1);
      Value *Shift = IRB.CreateBinOp(Instruction::Mul, Offset,
                                     ConstantInt::get(TargetIdxTy, 8));
      MaskVal =
          IRB.CreateBinOp(Instruction::Shl, ConstantInt::get(Cxt, Mask), Shift);
    } else {
      Mask.setBits(0, DestSz - 1);
      Value *Shift = IRB.CreateBinOp(Instruction::Mul, Offset,
                                     ConstantInt::get(TargetIdxTy, 8));
      MaskVal = ConstantInt::get(Cxt, Mask);
      Value *ShiftedMask = IRB.CreateBinOp(Instruction::Shl, MaskVal, Shift);
      Mask.setBits(0, SrcSz);
      MaskVal = IRB.CreateBinOp(Instruction::Xor, ShiftedMask,
                                ConstantInt::get(Cxt, Mask));
    }
  }
  return MaskVal;
}

Value *MemIntrinsicsHandler::GenMask(IRBuilder<> &IRB, Type *SrcTy, Type *DestTy,
                            Value *Offset, bool IsLoad) {
  const DataLayout &DL = Func->getParent()->getDataLayout();
  unsigned SrcSz = DL.getTypeSizeInBits(SrcTy);
  unsigned DestSz = DL.getTypeSizeInBits(DestTy);
  return GenMask(IRB, SrcSz, DestSz, Offset, IsLoad);
}

Value *MemIntrinsicsHandler::CreateGEP(IRBuilder<> &IRB, Value *I, unsigned i) {
  assert(isa<GetElementPtrInst>(I));
  GetElementPtrInst *GEP = cast<GetElementPtrInst>(I);
  Value *LastIndex = nullptr;
  SmallVector<Value *, 8> Indices;
  unsigned IdxCnt = 0;
  for (auto &Index : GEP->indices()) {
    if (IdxCnt++ == (GEP->getNumIndices() - 1)) {
      LastIndex = Index;
      break;
    }
    Indices.push_back(Index);
  }
  Value *NewIndex = IRB.CreateAdd(LastIndex, ConstantInt::get(TargetIdxTy, i));
  Indices.push_back(NewIndex);
  return IRB.CreateGEP(GEP->getPointerOperand(), Indices);
}

Value *MemIntrinsicsHandler::GetNextGEP(IRBuilder<> &IRB, Value *V) {
  TypeIndexVec WorkList;
  Value *NextGEP = nullptr;
  GetElementPtrInst *GEP = cast<GetElementPtrInst>(V);
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
        unsigned NextElemIdx = cast<ConstantInt>(PrevIndex)->getZExtValue() + 1;
        // For homogeneous type like array overrun is ok as it will be dealt
        // by run-time reference checking done by JVM but for heterogeneous type
        // like structure overrun cannot be done since type of overrun field 
        // cannot be determined.
        bool AllowOutOfBoundsAccesss = isa<ArrayType>(Ty) && !SrcMemPartOfStruct;
        if (NextElemIdx < NumElems || AllowOutOfBoundsAccesss) {
          NewLeafIndex = ConstantInt::get(TargetIdxTy, NextElemIdx);
          NewLeafType = cast<CompositeType>(Ty)->getTypeAtIndex(NewLeafIndex);
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
      GetIndicesForCompositeElemAccess(IRB, Indices, NewLeafType, &Offset);
    }
    NextGEP = IRB.CreateGEP(GEP->getPointerOperand(), Indices);
  }

  return NextGEP;
}

void MemIntrinsicsHandler::ProcessLoadStorePrimtive(IRBuilder<> &IRB, Value *SrcMem,
                                           Value *MemOp, Value *Offset) {
  Type *SrcTy = GetRootType(SrcMem->getType());
  Value *Ptr = isa<LoadInst>(MemOp)
                   ? cast<LoadInst>(MemOp)->getPointerOperand()
                   : cast<StoreInst>(MemOp)->getPointerOperand();
  // Destiniation type is used to compute the mask.
  Type *DestTy = cast<PointerType>(Ptr->getType())->getElementType();
  const DataLayout &DL = Func->getParent()->getDataLayout();
  unsigned DestTySz = DL.getTypeSizeInBits(DestTy);

  if (DL.getTypeSizeInBits(SrcTy) > DL.getTypeSizeInBits(DestTy)) {
    Value *LoadSrc = IRB.CreateLoad(SrcTy, SrcMem);
    Value *BitOffset = IRB.CreateMul(Offset, ConstantInt::get(TargetIdxTy, 8));
    BitOffset = IRB.CreateZExt(BitOffset, SrcTy);
    if (isa<LoadInst>(MemOp)) {
      LoadInst *LI = cast<LoadInst>(MemOp);
      Value *Mask = GenMask(IRB, SrcTy, DestTy, Offset, true);
      Value *Slice = IRB.CreateAnd(LoadSrc, Mask);
      Value *ShiftSlice = IRB.CreateLShr(Slice, BitOffset);
      Value *Truncate = IRB.CreateTrunc(ShiftSlice, DestTy);
      LI->replaceAllUsesWith(Truncate);
    } else {
      StoreInst *SI = cast<StoreInst>(MemOp);
      Value *Mask = GenMask(IRB, SrcTy, DestTy, Offset, false);
      Value *ClearWindow = IRB.CreateAnd(LoadSrc, Mask);
      Value *WidenStoreVal = IRB.CreateZExt(SI->getValueOperand(), SrcTy);
      Value *ShiftVal = IRB.CreateShl(WidenStoreVal, BitOffset);
      Value *MergeValue = IRB.CreateOr(ClearWindow, ShiftVal);
      Value *NewStore = IRB.CreateStore(MergeValue, SrcMem);
      SI->replaceAllUsesWith(NewStore);
    }
  } else {
    assert(isa<GetElementPtrInst>(SrcMem));

    int i = 0;
    unsigned MaskShift = 0;
    Value *LoadMask = nullptr;
    Value *StoreMask = nullptr;
    unsigned SrcTySz = DL.getTypeSizeInBits(SrcTy);
    if (isa<LoadInst>(MemOp)) {
      SmallVector<Value *, 4> PartialLoads;
      while (SrcMem && ((int)DestTySz > 0)) {
        Value *Load = IRB.CreateLoad(SrcMem);
        Value *WidenLoad = IRB.CreateZExt(Load, DestTy);
        DestTySz -= DL.getTypeSizeInBits(SrcTy);
        if (LoadMask) {
          Value *MaskedLoad = IRB.CreateAnd(WidenLoad, LoadMask);
          Value *ShiftedVal = IRB.CreateLShr(
              MaskedLoad, ConstantInt::get(DestTy, MaskShift));
          PartialLoads.push_back(ShiftedVal);
          break;
        } else {
          Value *ShiftVal = ConstantInt::get(WidenLoad->getType(), i++ * SrcTySz);
          Value *ShiftLoad = IRB.CreateShl(WidenLoad, ShiftVal);
          PartialLoads.push_back(ShiftLoad);
        }
        SrcMem = GetNextGEP(IRB, SrcMem);
        SrcTy = SrcMem ? GetRootType(SrcMem->getType()) : nullptr;
        if (SrcTy && DestTySz && DL.getTypeSizeInBits(SrcTy) > DestTySz) {
          SrcTySz = DL.getTypeSizeInBits(SrcTy);
          MaskShift = SrcTySz - DestTySz;
          LoadMask =
              GenMask(IRB, SrcTySz, DestTySz,
                      ConstantInt::get(TargetIdxTy, MaskShift / 8), true);
        }
      }
      // Perform reduction-OR between partial loads.
      while (PartialLoads.size() != 1) {
        Value *Load1 = PartialLoads.back();
        PartialLoads.pop_back();
        Value *Load2 = PartialLoads.back();
        PartialLoads.pop_back();
        Value *ReduceLoads = IRB.CreateOr(Load1, Load2);
        PartialLoads.push_back(ReduceLoads);
      }
      LoadInst *LI = cast<LoadInst>(MemOp);
      LI->replaceAllUsesWith(PartialLoads[0]);
    } else {
      Value *StoreVal = cast<StoreInst>(MemOp)->getValueOperand();
      while (SrcMem && ((int)DestTySz > 0)) {
        DestTySz -= DL.getTypeSizeInBits(SrcTy);
        if (StoreMask) {
          // Partial write to final SrcMem GEP.
          Value *PartialNewContents = IRB.CreateAnd(StoreVal, LoadMask);
          Value *LoadMem = IRB.CreateLoad(SrcMem);
          Value *OldMemContents = IRB.CreateAnd(LoadMem, StoreMask);
          Value *NewContents = IRB.CreateOr(OldMemContents, PartialNewContents);
          IRB.CreateStore(NewContents, SrcMem);
          break;
        } else {
          Value *ShiftVal = ConstantInt::get(StoreVal->getType(), i++ * SrcTySz);
          Value *ShiftLoad = IRB.CreateLShr(StoreVal, ShiftVal);
          Value *NewContents = IRB.CreateTrunc(ShiftLoad, SrcTy);
          IRB.CreateStore(NewContents, SrcMem);
        }
        SrcMem = GetNextGEP(IRB, SrcMem);
        SrcTy = SrcMem ? GetRootType(SrcMem->getType()) : nullptr;
        if (SrcTy && DestTySz && DL.getTypeSizeInBits(SrcTy) > DestTySz) {
          SrcTySz = DL.getTypeSizeInBits(SrcTy);
          StoreMask = GenMask(IRB, SrcTySz, DestTySz,
                              ConstantInt::get(TargetIdxTy, 0), false);
          LoadMask = GenMask(
              IRB, SrcTySz, DestTySz,
              ConstantInt::get(TargetIdxTy, (SrcTySz - DestTySz) / 8), true);
        }
      }
    }
  }
  AddToLazyDeletionSet(cast<User>(MemOp));
}

void MemIntrinsicsHandler::ProcessMemOpWRTSourcePrimitive(Value *SrcMem,
                                                 SrcMemOpOffset &MemOpOffsetMap,
                                                 Type *DestTy) {
  for (auto &MemOpOffsetPair : MemOpOffsetMap) {
    IRBuilder<> IRB(cast<Instruction>(MemOpOffsetPair.first));
    ProcessLoadStorePrimtive(IRB, SrcMem, MemOpOffsetPair.first,
                             MemOpOffsetPair.second);
  }
}

void MemIntrinsicsHandler::ProcessPrimitive(Type *SrcTy, Type *DestTy, Value *Offset,
                                   Value *SrcMem, BitCastInst &I) {
  assert(SrcTy->isIntegerTy());

  SrcMemOpOffset MemOpOffsetMap;
  for (auto *User : I.users())
    ProcessUser(MemOpOffsetMap, *User, Offset, false);

  ProcessMemOpWRTSourcePrimitive(SrcMem, MemOpOffsetMap, DestTy);
}

unsigned MemIntrinsicsHandler::GetStructElmIndexAtOffset(StructType *STy,
                                                unsigned Offset,
                                                unsigned &PadOffset) {
  unsigned i = 0;
  const DataLayout &DL = Func->getParent()->getDataLayout();
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
  if (SL->hasPadding() && (Offset - SL->getElementOffset(ElmIdx)) > ElmSz) {
    unsigned Alignment = SL->getAlignment();
    PadOffset = Alignment - ((Offset - SL->getElementOffset(ElmIdx)));
  }
  return ElmIdx;
}

void MemIntrinsicsHandler::GetIndicesForCompositeElemAccess(
    IRBuilder<> &IRB, SmallVectorImpl<Value *> &Indices, Type *CompType,
    Value **Offset) {
  const DataLayout &DL = Func->getParent()->getDataLayout();
  if (isa<StructType>(CompType)) {
    assert(isa<ConstantInt>(*Offset) &&
           "Variable offset used for field access");
    unsigned PadOffset = 0;
    unsigned ConstOffset = cast<ConstantInt>(*Offset)->getZExtValue();
    StructType *STy = cast<StructType>(CompType);
    const StructLayout *SL = DL.getStructLayout(STy);
    int i = GetStructElmIndexAtOffset(STy, ConstOffset, PadOffset);
    Indices.push_back(ConstantInt::get(TargetIdxTy, i));
    unsigned ElmOffset = SL->getElementOffset(i);
    Value *ElmOffsetVal = ConstantInt::get(TargetIdxTy, ElmOffset);
    *Offset = IRB.CreateBinOp(Instruction::Sub, *Offset, ElmOffsetVal);
    if (isa<CompositeType>(STy->getTypeAtIndex(i)))
      GetIndicesForCompositeElemAccess(IRB, Indices, STy->getTypeAtIndex(i),
                                       Offset);
  } else if (isa<ArrayType>(CompType)) {
    Type *ElmTy = cast<ArrayType>(CompType)->getElementType();
    Value *ElmSz =
        ConstantInt::get(TargetIdxTy, DL.getTypeSizeInBits(ElmTy) / 8);
    Value *Index = IRB.CreateBinOp(Instruction::UDiv, *Offset, ElmSz);
    Indices.push_back(Index);
    *Offset = IRB.CreateBinOp(Instruction::URem, *Offset, ElmSz);
    if (isa<CompositeType>(ElmTy))
      GetIndicesForCompositeElemAccess(IRB, Indices, ElmTy, Offset);
  } else {
    Value *CompTypeSz =
        ConstantInt::get(TargetIdxTy, DL.getTypeSizeInBits(CompType) / 8);
    Value *Index = IRB.CreateBinOp(Instruction::UDiv, *Offset, CompTypeSz);
    Indices.push_back(Index);
    *Offset = IRB.CreateBinOp(Instruction::Sub, *Offset, CompTypeSz);
  }
}

Value *MemIntrinsicsHandler::CreateCompositeElemAccessGEP(IRBuilder<> &IRB, Value *Mem,
                                                 Type *CompType,
                                                 Value **Offset) {
  SmallVector<Value *, 4> Indices;
  Indices.push_back(ConstantInt::get(TargetIdxTy, 0));
  GetIndicesForCompositeElemAccess(IRB, Indices, CompType, Offset);
  return IRB.CreateGEP(Mem, Indices);
}

void MemIntrinsicsHandler::ProcessMemOpWRTCompositeSource(Value *SrcMem,
                                                 SrcMemOpOffset &MemOpOffsetMap,
                                                 Type *DestTy) {
  Type *SrcTy = GetRootType(SrcMem->getType());
  assert(isa<ArrayType>(SrcTy) || isa<StructType>(SrcTy));

  for (auto &MemOpOffsetPair : MemOpOffsetMap) {
    IRBuilder<> IRB(cast<Instruction>(MemOpOffsetPair.first));
    Value *Offset = MemOpOffsetPair.second;
    Value *SrcElement =
        CreateCompositeElemAccessGEP(IRB, SrcMem, SrcTy, &Offset);
    ProcessLoadStorePrimtive(IRB, SrcElement, MemOpOffsetPair.first, Offset);
  }
}

void MemIntrinsicsHandler::ProcessArrays(Type *SrcTy, Type *DestTy, Value *Offset,
                                Value *SrcMem, BitCastInst &I) {
  SrcMemOpOffset MemOpOffsetMap;
  for (auto *User : I.users())
    ProcessUser(MemOpOffsetMap, *User, Offset, false);

  ProcessMemOpWRTCompositeSource(SrcMem, MemOpOffsetMap, DestTy);
}

void MemIntrinsicsHandler::ProcessStructures(Type *SrcTy, Type *DestTy, Value *Offset,
                                    Value *SrcMem, BitCastInst &I) {

  SrcMemOpOffset MemOpOffsetMap;
  for (auto *User : I.users())
    ProcessUser(MemOpOffsetMap, *User, Offset, false);

  ProcessMemOpWRTCompositeSource(SrcMem, MemOpOffsetMap, DestTy);
}

void MemIntrinsicsHandler::visitBitCastInst(BitCastInst &I) {
  bool HasBitCastInChain = false;
  Value *Offset = ConstantInt::get(TargetIdxTy, 0);

  Value *Root = GetRootObject(&I, &Offset, HasBitCastInChain);
  assert(Root && isa<PointerType>(Root->getType()) &&
         "Cannot find root pointer object from bitcast");

  // Skip the processing of bitcast if it has another
  // bitcast (parent) in the chain of instructions from
  // RootObject. Offset computation of its users will be
  // taken into consideration while computing the offset
  // of parent bitcast.
  if (HasBitCastInChain)
    return;

  Type *RootTy = GetRootType(Root->getType());
  Type *DestTy = GetRootType(I.getDestTy());

  if (isa<StructType>(RootTy))
    ProcessStructures(RootTy, DestTy, Offset, Root, I);
  else if (isa<ArrayType>(RootTy))
    ProcessArrays(RootTy, DestTy, Offset, Root, I);
  else
    ProcessPrimitive(RootTy, DestTy, Offset, Root, I);

  AddToLazyDeletionSet(cast<User>(&I));
}

void MemIntrinsicsHandler::AddToLazyDeletionSet(User *Elm) {
  if (DeletionSet.find(Elm) == DeletionSet.end())
    DeletionSet.insert(Elm);
}

void MemIntrinsicsHandler::PopulateEraseUsers(User *I, SmallVectorImpl<User *> &Users) {
  for (auto *User : I->users()) {
    if (User->getNumUses())
      PopulateEraseUsers(User, Users);
    Users.push_back(User);
  }
}

void MemIntrinsicsHandler::ProcessLazyDeletionSet() {
  for (auto *Val : DeletionSet) {
    SmallVector<User *, 8> Users;
    PopulateEraseUsers(Val, Users);
    Users.push_back(Val);
    for (auto *I : Users) {
      cast<Instruction>(I)->eraseFromParent();
      if (Val != I && DeletionSet.find(I) != DeletionSet.end())
        DeletionSet.erase(I);
    }
  }
}

void MemIntrinsicsHandler::ProcessFunction() {
  LLVMContext &Cxt = Func->getContext();
  const DataLayout &DL = Func->getParent()->getDataLayout();
  TargetIdxTy = GetTargetIndexType(DL, Cxt);
  visit(Func);
  ProcessLazyDeletionSet();
}
} // namespace llvm
