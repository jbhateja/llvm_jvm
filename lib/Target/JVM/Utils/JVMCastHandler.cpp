//===------------------ JVMCastHandler.cpp---------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
// This file contains utility class to handle bitcast (re-interpret) cast
// instruction. All the references to destination types needs to be
// transformed to source types and must use source(root) memory.
//===----------------------------------------------------------------------===//

#include "JVMCastHandler.h"
#include "llvm/CodeGen/JVMCommonUtils.h"
#include "llvm/IR/ConstantFolder.h"

#define DEBUG_TYPE "cast-handler"

using namespace llvm;

namespace llvm {

bool CastHandler::ProcessUser(SrcMemOpOffset &MemOpOffsetMap, User &I,
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

bool CastHandler::ProcessGEPChain(SrcMemOpOffset &MemOpOffsetMap,
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

Value *CastHandler::GenMask(IRBuilder<> &IRB, Type *SrcTy, Type *DestTy,
                            Value *Offset, bool IsLoad) {
  LLVMContext &Cxt = Func->getContext();
  const DataLayout &DL = Func->getParent()->getDataLayout();
  unsigned SrcSz = DL.getTypeSizeInBits(SrcTy);
  unsigned DestSz = DL.getTypeSizeInBits(DestTy);
  return llvm::GenMask(Cxt, DL, IRB, SrcSz, DestSz, Offset, IsLoad);
}

void CastHandler::ProcessLoadStorePrimtive(IRBuilder<> &IRB, Value *SrcMem,
                                           Value *MemOp, Value *Offset,
                                           unsigned Align) {
  LLVMContext &Cxt = Func->getContext();
  const DataLayout &DL = Func->getParent()->getDataLayout();
  Type *SrcTy = GetRootType(SrcMem->getType());
  Value *Ptr = isa<LoadInst>(MemOp)
                   ? cast<LoadInst>(MemOp)->getPointerOperand()
                   : cast<StoreInst>(MemOp)->getPointerOperand();
  // Destiniation type is used to compute the mask.
  Type *DestTy = cast<PointerType>(Ptr->getType())->getElementType();
  unsigned DestTySz = DL.getTypeSizeInBits(DestTy);

  if (DL.getTypeSizeInBits(SrcTy) > DL.getTypeSizeInBits(DestTy)) {
    Value *LoadSrc = IRB.CreateLoad(SrcTy, SrcMem);
    Value *BitOffset = IRB.CreateMul(Offset, ConstantInt::get(TargetIdxTy, 8));
    BitOffset = GetTypeNormalizedValue(DL, IRB, BitOffset, SrcTy);
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
        unsigned Padding = 0;
        Value *Load = IRB.CreateLoad(SrcTy, SrcMem);
        Value *WidenLoad = IRB.CreateZExt(Load, DestTy);
        DestTySz -= DL.getTypeSizeInBits(SrcTy);
        if (LoadMask) {
          Value *MaskedLoad = IRB.CreateAnd(WidenLoad, LoadMask);
          Value *ShiftedVal =
              IRB.CreateLShr(MaskedLoad, ConstantInt::get(DestTy, MaskShift));
          PartialLoads.push_back(ShiftedVal);
          break;
        } else {
          unsigned Shift = i++ * SrcTySz;
          Shift = Align ? ((Shift + Align - 1) / Align) * Align : Shift;
          Value *ShiftVal = ConstantInt::get(WidenLoad->getType(), Shift);
          Value *ShiftLoad = IRB.CreateShl(WidenLoad, ShiftVal);
          PartialLoads.push_back(ShiftLoad);
        }
        SrcMem = GetNextGEP(DL, IRB, TargetIdxTy, SrcMem, Padding);
        DestTySz -= Align ? (DestTySz % Align) : 0;
        SrcTy = SrcMem ? GetRootType(SrcMem->getType()) : nullptr;
        if (SrcTy && DestTySz && DL.getTypeSizeInBits(SrcTy) > DestTySz) {
          SrcTySz = DL.getTypeSizeInBits(SrcTy);
          MaskShift = SrcTySz - DestTySz;
          LoadMask =
              llvm::GenMask(Cxt, DL, IRB, SrcTySz, DestTySz,
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
        unsigned Padding = 0;
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
          unsigned Shift = i++ * SrcTySz;
          Shift = Align ? ((Shift + Align - 1) / Align) * Align : Shift;
          Value *ShiftVal = ConstantInt::get(StoreVal->getType(), Shift);
          Value *ShiftLoad = IRB.CreateLShr(StoreVal, ShiftVal);
          Value *NewContents = IRB.CreateTrunc(ShiftLoad, SrcTy);
          IRB.CreateStore(NewContents, SrcMem);
        }
        SrcMem = GetNextGEP(DL, IRB, TargetIdxTy, SrcMem, Padding);
        DestTySz -= Align ? (DestTySz % Align) : 0;
        SrcTy = SrcMem ? GetRootType(SrcMem->getType()) : nullptr;
        if (SrcTy && DestTySz && DL.getTypeSizeInBits(SrcTy) > DestTySz) {
          SrcTySz = DL.getTypeSizeInBits(SrcTy);
          StoreMask = llvm::GenMask(Cxt, DL, IRB, SrcTySz, DestTySz,
                              ConstantInt::get(TargetIdxTy, 0), false);
          LoadMask = llvm::GenMask(
              Cxt, DL, IRB, SrcTySz, DestTySz,
              ConstantInt::get(TargetIdxTy, (SrcTySz - DestTySz) / 8), true);
        }
      }
    }
  }
  AddToLazyDeletionSet(cast<User>(MemOp));
}

void CastHandler::ProcessMemOpWRTSourcePrimitive(Value *SrcMem,
                                                 SrcMemOpOffset &MemOpOffsetMap,
                                                 Type *DestTy) {
  for (auto &MemOpOffsetPair : MemOpOffsetMap) {
    IRBuilder<> IRB(cast<Instruction>(MemOpOffsetPair.first));
    ProcessLoadStorePrimtive(IRB, SrcMem, MemOpOffsetPair.first,
                             MemOpOffsetPair.second, 0);
  }
}

void CastHandler::ProcessPrimitive(Type *SrcTy, Type *DestTy, Value *Offset,
                                   Value *SrcMem, BitCastInst &I) {
  assert(SrcTy->isIntegerTy());

  SrcMemOpOffset MemOpOffsetMap;
  for (auto *User : I.users())
    ProcessUser(MemOpOffsetMap, *User, Offset, false);

  ProcessMemOpWRTSourcePrimitive(SrcMem, MemOpOffsetMap, DestTy);
}


void CastHandler::ProcessMemOpWRTCompositeSource(Value *SrcMem,
                                                 SrcMemOpOffset &MemOpOffsetMap,
                                                 Type *DestTy, unsigned Align) {
  Type *SrcTy = GetRootType(SrcMem->getType());
  const DataLayout &DL = Func->getParent()->getDataLayout();
  assert(isa<ArrayType>(SrcTy) || isa<StructType>(SrcTy));

  for (auto &MemOpOffsetPair : MemOpOffsetMap) {
    IRBuilder<> IRB(cast<Instruction>(MemOpOffsetPair.first));
    Value *Offset = MemOpOffsetPair.second;
    Value *SrcElement =
        llvm::CreateCompositeElemAccessGEP(DL, IRB, SrcMem, TargetIdxTy, SrcTy, &Offset);
    ProcessLoadStorePrimtive(IRB, SrcElement, MemOpOffsetPair.first, Offset,
                             Align);
  }
}

void CastHandler::ProcessArrays(Type *SrcTy, Type *DestTy, Value *Offset,
                                Value *SrcMem, BitCastInst &I) {
  SrcMemOpOffset MemOpOffsetMap;
  for (auto *User : I.users())
    ProcessUser(MemOpOffsetMap, *User, Offset, false);

  ProcessMemOpWRTCompositeSource(SrcMem, MemOpOffsetMap, DestTy, 0);
}

void CastHandler::ProcessStructures(Type *SrcTy, Type *DestTy, Value *Offset,
                                    Value *SrcMem, BitCastInst &I) {
  assert(isa<StructType>(SrcTy) && "StructType expected");
  const DataLayout &DL = Func->getParent()->getDataLayout();
  const StructLayout *SL = DL.getStructLayout(cast<StructType>(SrcTy));
  unsigned Align = SL->hasPadding() ? SL->getAlignment() * 8 : 0;

  SrcMemOpOffset MemOpOffsetMap;
  for (auto *User : I.users())
    ProcessUser(MemOpOffsetMap, *User, Offset, false);

  ProcessMemOpWRTCompositeSource(SrcMem, MemOpOffsetMap, DestTy, Align);
}

void CastHandler::visitBitCastInst(BitCastInst &I) {
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

void CastHandler::AddToLazyDeletionSet(User *Elm) {
  if (DeletionSet.find(Elm) == DeletionSet.end())
    DeletionSet.insert(Elm);
}

void CastHandler::PopulateEraseUsers(User *I, SetVector<User *> &Users) {
  for (auto *User : I->users()) {
    if (User->getNumUses())
      PopulateEraseUsers(User, Users);
    Users.insert(User);
  }
}

void CastHandler::ProcessLazyDeletionSet() {
  SetVector<User *> Users;
  for (auto *Val : DeletionSet) {
    PopulateEraseUsers(Val, Users);
    Users.insert(Val);
  } 
  for (auto *I : Users)
    cast<Instruction>(I)->eraseFromParent();
  DeletionSet.clear();
}


void CastHandler::ProcessFunction() {
  LLVMContext &Cxt = Func->getContext();
  const DataLayout &DL = Func->getParent()->getDataLayout();
  TargetIdxTy = GetTargetIndexType(DL, Cxt);
  visit(Func);
  ProcessLazyDeletionSet();
}
} // namespace llvm
