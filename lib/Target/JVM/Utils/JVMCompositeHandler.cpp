//===------------- JVMCompositeHandler.cpp---------------------------------===//
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

#include "JVMCompositeHandler.h"
#include "llvm/CodeGen/JVMCommonUtils.h"
#include "llvm/Transforms/Utils/LowerMemIntrinsics.h"

#define DEBUG_TYPE "composite-handler"

using namespace llvm;

namespace llvm {

static void InsertIntoSet(CompositeHandler::ValueSet &Set, Value *Val) {
  if (Set.find(Val) == Set.end())
    Set.insert(Val);
}

static bool CheckIfFlattendType(Type *Ty) {
  bool retVal = true;
  if (StructType *STy = dyn_cast<StructType>(Ty)) {
    for (StructType::element_iterator EB = STy->element_begin(), EI = EB,
                                      EE = STy->element_end();
         EI != EE; ++EI) {
      if (isa<StructType>(*EI))
        return false;
      else if (isa<ArrayType>(*EI))
        retVal &= CheckIfFlattendType(*EI);
    }
  } else if (dyn_cast<PointerType>(Ty)) {
    return CheckIfFlattendType(cast<PointerType>(Ty)->getElementType());
  } else if (dyn_cast<ArrayType>(Ty)) {
    Type *ElmTy = cast<ArrayType>(Ty)->getElementType();
    if (isa<StructType>(ElmTy))
      return false;
    else if (CompositeType::classof(ElmTy))
      retVal &= CheckIfFlattendType(ElmTy);
  }
  return retVal;
}

static unsigned GetNumLeafArrayDimensions(GetElementPtrInst *V) {
  int i = 0;
  SmallVector<Type *, 8> WorkList;
  for (gep_type_iterator Itr = gep_type_begin(V); Itr != gep_type_end(V); Itr++)
    WorkList.push_back(Itr.getIndexedType());

  WorkList.pop_back();
  while (WorkList.size()) {
    Type *Ty = WorkList.back();
    if (!isa<ArrayType>(Ty))
      break;
    WorkList.pop_back();
    i++;
  }
  return i;
}

static unsigned GetNumLeadingNonLeafArrDim(GetElementPtrInst *GI) {
  if (GI->getNumIndices() > 2)
    return (GI->getNumIndices() - (GetNumLeafArrayDimensions(GI) + 1));
  else
    return 0;
}

static void CollectPrimitiveTypes(Type *Ty, CompositeHandler::TypeVec &TyVec) {
  // Given a struct type, recursively traverse the elements.
  if (StructType *STy = dyn_cast<StructType>(Ty)) {
    for (StructType::element_iterator EB = STy->element_begin(), EI = EB,
                                      EE = STy->element_end();
         EI != EE; ++EI) {
      CollectPrimitiveTypes(*EI, TyVec);
    }
  } else if (dyn_cast<PointerType>(Ty)) {
    CollectPrimitiveTypes(cast<PointerType>(Ty)->getElementType(), TyVec);
  }
  // Given an array type, traverse element type.
  else if (dyn_cast<ArrayType>(Ty)) {
    Type *ElmTy = cast<ArrayType>(Ty)->getElementType();
    if (isa<StructType>(ElmTy) ||
        (isa<ArrayType>(ElmTy) && !CheckIfFlattendType(ElmTy))) {
      CompositeHandler::TypeVec ElmSubTys;
      CollectPrimitiveTypes(ElmTy, ElmSubTys);
      for (unsigned i = 0; i < cast<ArrayType>(Ty)->getNumElements(); i++)
        TyVec.append(ElmSubTys.begin(), ElmSubTys.end());
    } else
      TyVec.push_back(Ty);
  } else
    TyVec.push_back(Ty);
}

static Type *GetMostPreferredTypeForStruct(LLVMContext &Cxt,
                                           CompositeHandler::TypeVec &TyVec,
                                           int &NumElements) {
  Type *MPT = Type::getInt32Ty(Cxt);
  for (auto Ty : TyVec) {
    int ElmCnt = 0;
    Type *ElmTy = nullptr;
    if (isa<ArrayType>(Ty)) {
      ElmCnt = cast<ArrayType>(Ty)->getNumElements();
      ElmTy = cast<ArrayType>(Ty)->getElementType();
      while (isa<ArrayType>(ElmTy)) {
        ElmCnt *= cast<ArrayType>(ElmTy)->getNumElements();
        ElmTy = cast<ArrayType>(ElmTy)->getElementType();
      }
    } else {
      ElmTy = Ty;
      ElmCnt = 1;
    }

    // TODO:Split the Type into multiples of MPT and record their corresponding
    // fields as instructions operating on those fields needs to be handled
    // appropriately (split/expanded).
    if (ElmTy->getPrimitiveSizeInBits() > MPT->getPrimitiveSizeInBits()) {
      ElmCnt *= ElmTy->getPrimitiveSizeInBits() / MPT->getPrimitiveSizeInBits();
      llvm_unreachable("Encountered a type greater than MPT(i32)");
    }

    NumElements += ElmCnt;
  }
  return MPT;
}

static void CopyArgumentAttributes(Argument *Old, Function *Func, int ArgNo) {
  if (Old->hasNonNullAttr())
    Func->addParamAttr(ArgNo, Attribute::NonNull);
  if (Old->hasByValAttr())
    Func->addParamAttr(ArgNo, Attribute::ByVal);
  if (Old->hasSwiftSelfAttr())
    Func->addParamAttr(ArgNo, Attribute::SwiftSelf);
  if (Old->hasSwiftErrorAttr())
    Func->addParamAttr(ArgNo, Attribute::SwiftError);
  if (Old->hasInAllocaAttr())
    Func->addParamAttr(ArgNo, Attribute::InAlloca);
  if (Old->hasNestAttr())
    Func->addParamAttr(ArgNo, Attribute::Nest);
  if (Old->hasNoAliasAttr())
    Func->addParamAttr(ArgNo, Attribute::NoAlias);
  if (Old->hasNoCaptureAttr())
    Func->addParamAttr(ArgNo, Attribute::NoCapture);
  if (Old->hasStructRetAttr())
    Func->addParamAttr(ArgNo, Attribute::StructRet);
  if (Old->hasReturnedAttr())
    Func->addParamAttr(ArgNo, Attribute::Returned);
  if (Old->hasZExtAttr())
    Func->addParamAttr(ArgNo, Attribute::ZExt);
  if (Old->hasSExtAttr())
    Func->addParamAttr(ArgNo, Attribute::SExt);
}

static Argument *BuildAndSetArgsList(Function *Func) {
  // Create the arguments vector, all arguments start out unnamed.
  int ArgsCnt = Func->arg_size();
  Argument *Arguments = Func->arg_begin();
  if (ArgsCnt > 0) {
    Argument *NewArguments = std::allocator<Argument>().allocate(ArgsCnt);
    Func->setArguments(NewArguments, ArgsCnt);

    for (unsigned i = 0, e = ArgsCnt; i != e; ++i) {
      Type *ArgTy = Func->getFunctionType()->getParamType(i);
      assert(!ArgTy->isVoidTy() && "Cannot have void typed arguments!");
      new (NewArguments + i)
          Argument(ArgTy, "", const_cast<Function *>(Func), i);
      CopyArgumentAttributes(Arguments + i, Func, i);
    }
  }
  return Arguments;
}

static void CopyScalarObj(Value *Dst, Value *Src, Type *ElmTy,
                          IRBuilder<> &IRB) {
  Value *SrcLoad = IRB.CreateLoad(Src);
  IRB.CreateGEP(SrcLoad, Dst);
}

static void CopyCompositeObj(Value *DstBase, Value *SrcBase, Type *Ty,
                             int NumElms, CompositeHandler::ValueVec &IdxVec,
                             IRBuilder<> &IRB, LLVMContext &Cxt) {
  for (int i = 0; i < NumElms; i++) {
    assert(
        !isa<PointerType>(Ty) && !isa<StructType>(Ty) &&
        "Unexpected element of type pointer/structure found while object copy");
    Type *ElmTy = cast<SequentialType>(Ty)->getElementType();
    if (isa<ArrayType>(ElmTy)) {
      IdxVec.push_back(ConstantInt::get(Type::getInt32Ty(Cxt), APInt(32, i)));
      CopyCompositeObj(
          DstBase, SrcBase, cast<ArrayType>(ElmTy)->getElementType(),
          cast<ArrayType>(ElmTy)->getNumElements(), IdxVec, IRB, Cxt);
      IdxVec.pop_back();
    } else {
      IdxVec.push_back(ConstantInt::get(Type::getInt32Ty(Cxt), APInt(32, i)));

      Value *SrcGEP = IRB.CreateGEP(SrcBase, IdxVec);
      Value *SrcLoad = IRB.CreateLoad(SrcGEP);
      Value *DstGEP = IRB.CreateGEP(DstBase, IdxVec);
      IRB.CreateStore(SrcLoad, DstGEP);

      // TODO : Memory deallocation for popped constant.
      IdxVec.pop_back();
    }
  }
}

void CompositeHandler::dump() {
  for (auto Ty : typeDefs) {
    DEBUG({ 
     dbgs() << "Type : ";
     Ty.first->dump();
     dbgs() << "Values:";
     for (auto Val : typeValues[Ty.first])
        Val->dump();
     dbgs() << "Flattend Type :";
     if (Ty.second)
       Ty.second->dump();
     else
      dbgs() << "<null>";
     dbgs() << "\n";
    });
  }
}

bool CompositeHandler::canFlattenType(Type *Ty) {
  if (Ty->isPointerTy())
    Ty = cast<PointerType>(Ty)->getElementType();

  if (CompositeType::classof(Ty) &&
      (dyn_cast<StructType>(Ty) ||
       (dyn_cast<ArrayType>(Ty) &&
        !CheckIfFlattendType(cast<ArrayType>(Ty)->getElementType()))))
    return true;
  return false;
}

void CompositeHandler::visitValue(Value *V, Type *VT) {
  if (canFlattenType(VT)) {
    if (typeDefs.find(VT) == typeDefs.end())
      typeDefs[VT] = nullptr;
    typeValues[VT].push_back(V);
  }
}

GetElementPtrInst *
CompositeHandler::extractIndices(Type *Ty, GetElementPtrInst *Elm,
                                 ValueVec &IList, ValueSet &LazyRemovalSet,
                                 bool AccessMemFormFlatStruct) {
  if (!Elm->hasOneUse())
    return nullptr;

  Ty = GetRootType(Ty);
  if (Elm->getNumIndices() > 2 && AccessMemFormFlatStruct) {
    unsigned NumLeadingNonLeafArrDim = GetNumLeadingNonLeafArrDim(Elm);
    IList.append(Elm->idx_begin(),
                 Elm->idx_begin() + NumLeadingNonLeafArrDim + 1);
  } else
    IList.append(Elm->idx_begin(), Elm->idx_end());

  User *OnlyUse = *(Elm->user_begin());
  if (dyn_cast<GetElementPtrInst>(OnlyUse)) {
    InsertIntoSet(LazyRemovalSet, Elm);
    return extractIndices(Ty, cast<GetElementPtrInst>(OnlyUse), IList,
                          LazyRemovalSet, AccessMemFormFlatStruct);
  }
  return Elm;
}

Type *CompositeHandler::flattenType_core(LLVMContext &Cxt, Type *Ty) {
  if (CheckIfFlattendType(Ty))
    return Ty;

  CompositeHandler::TypeVec TyVec;
  CollectPrimitiveTypes(Ty, TyVec);

  // If any of the uses of structure type objects use non-constant indices w.r.t
  // object or is an argument, then allocate an array corresponding to most
  // preferred type (MPT). MPT is integer since JVM LocalVariable array and
  // operand stack work at the granularity of integer.

  // Not allocating array in each case gives the freedom to type blast structure
  // into individual element which can be allocated independently.
  bool AllocateArrayOfMPT = false;
  for (auto TyObj : typeValues[Ty])
    if (!AreAllUsesConstantIndexedReferences(TyObj) || isa<Argument>(TyObj))
      AllocateArrayOfMPT = true;

  Type *NewTy;
  if (AllocateArrayOfMPT) {
    // TODO: Boundary check for structure having just one scalar
    // i.e. when NumElements = 1
    int NumElements = 0;
    Type *MPT =
        GetMostPreferredTypeForStruct(Func->getContext(), TyVec, NumElements);
    NewTy = ArrayType::get(MPT, NumElements);
    typeIsBlasted[Ty] = false;
  } else {
    NewTy = StructType::get(Cxt, TyVec);
    typeIsBlasted[Ty] = true;
  }

  if (Ty->isPointerTy())
    return PointerType::get(NewTy, cast<PointerType>(Ty)->getAddressSpace());
  else
    return NewTy;
}

void CompositeHandler::flattenTypes() {
  for (auto &TyPair : typeDefs) {
    assert(typeDefs.find(TyPair.first) != typeDefs.end());

    if (typeDefs[TyPair.first] == nullptr)
      typeDefs[TyPair.first] =
          flattenType_core(Func->getContext(), TyPair.first);
  }
}

bool CompositeHandler::isBlastedType(Type *Ty) {
  assert(typeIsBlasted.find(Ty) != typeIsBlasted.end());
  return typeIsBlasted[Ty];
}

bool CompositeHandler::AreAllUsesConstantIndexedReferences(Value *I) {
  for (auto *User : I->users()) {
    if (!isa<GetElementPtrInst>(User))
      return false;
    else {
      // TODO: Leaf array dimensions could contain variable indices.
      GetElementPtrInst *GI = cast<GetElementPtrInst>(User);
      for (auto &Idx : GI->indices())
        if (!isa<ConstantInt>(Idx))
          return false;
    }
  }
  return true;
}

// Following function returns total number of elements in the given type,
// when AccessMemFormFlatStruct is true it considers an array of primitive
// type as one element while computing number of elements.

int CompositeHandler::getNumElements(Type *Ty, bool AccessMemFormFlatStruct) {
  int NumElems = 0;

  Type *TyKey = reinterpret_cast<Type *>(reinterpret_cast<char *>(Ty) +
                                         (int)AccessMemFormFlatStruct);
  if (typeNumElemCache.find(TyKey) != typeNumElemCache.end())
    return typeNumElemCache[TyKey];

  if (isa<StructType>(Ty)) {
    StructType *STy = cast<StructType>(Ty);
    for (StructType::element_iterator EB = STy->element_begin(), EI = EB,
                                      EE = STy->element_end();
         EI != EE; ++EI) {
      if (CompositeType::classof(*EI))
        NumElems += getNumElements(*EI, AccessMemFormFlatStruct);
      else
        NumElems += 1;
    }
  } else if (isa<ArrayType>(Ty)) {
    Type *ElmTy = cast<ArrayType>(Ty)->getElementType();
    bool exploreArr = !CheckIfFlattendType(ElmTy) || !AccessMemFormFlatStruct;
    if (isa<StructType>(ElmTy) || (isa<ArrayType>(ElmTy) && exploreArr)) {
      int NumArrElems = cast<ArrayType>(Ty)->getNumElements();
      NumElems += NumArrElems * getNumElements(ElmTy, AccessMemFormFlatStruct);
    } else
      NumElems += exploreArr ? cast<ArrayType>(Ty)->getNumElements() : 1;
  } else if (isa<PointerType>(Ty)) {
    NumElems += getNumElements(cast<PointerType>(Ty)->getElementType(),
                               AccessMemFormFlatStruct);
  } else
    NumElems += 1;

  typeNumElemCache[TyKey] = NumElems;
  return NumElems;
}

Value *
CompositeHandler::getTyIndexInFlattenedType_core(Function *Func, Value *Idx,
                                                 GetElementPtrInst *GI,
                                                 bool AccessMemFormFlatStruct) {
  using NextIndexRefKind = enum { StructFieldRef, ArrayElemRef, ScalarRef };
  NextIndexRefKind NextRef = ScalarRef;

  Type *PrevTy = nullptr;
  Type *Int32Ty = Type::getInt32Ty(Func->getContext());
  assert(GI->hasOneUse() && "GEP has more than one users");

  gep_type_iterator IdxTyItr = gep_type_begin(GI);

  IRBuilder<> IRB(GI);
  unsigned NumLeadingNonLeafArrDim = AccessMemFormFlatStruct
                                         ? GetNumLeadingNonLeafArrDim(GI)
                                         : GI->getNumIndices();
  for (; IdxTyItr != gep_type_end(GI); IdxTyItr++) {
    Value *IdxOffset = nullptr;
    Type *IdxTy = IdxTyItr.getIndexedType();
    Value *IdxVal = IdxTyItr.getOperand();

    Type *ExpTy = IdxVal->getType();
    // Insert a truncation operation to extract 32 bit index.
    // Ideally max size to chosen for coercing of expression.
    if (ExpTy->getPrimitiveSizeInBits() == 64) {
      IRB.SetInsertPoint(GI);
      IdxVal = IRB.CreateTrunc(IdxVal, Int32Ty);
    }

    if (NextRef == StructFieldRef) {
      unsigned CumulativeIndex = 0;
      assert(isa<ConstantInt>(IdxVal) && "Field index must be a constant");
      for (unsigned i = 0; i < cast<ConstantInt>(IdxVal)->getZExtValue(); i++)
        CumulativeIndex +=
            getNumElements(cast<StructType>(PrevTy)->getElementType(i),
                           AccessMemFormFlatStruct);
      IdxOffset = ConstantInt::get(Int32Ty, CumulativeIndex);
    } else if (NextRef == ArrayElemRef) {
      // Array index could be a constant or a variable.
      IdxOffset = IRB.CreateBinOp(
          Instruction::Mul,
          ConstantInt::get(
              Int32Ty, APInt(32, getNumElements(
                                     cast<ArrayType>(PrevTy)->getElementType(),
                                     AccessMemFormFlatStruct))),
          IdxVal);
    } else
      IdxOffset = IdxVal;

    if (isa<ArrayType>(IdxTy))
      NextRef = ArrayElemRef;
    else if (isa<StructType>(IdxTy) || isa<PointerType>(IdxTy))
      NextRef = StructFieldRef;
    else
      NextRef = ScalarRef;

    Idx = IRB.CreateBinOp(Instruction::Add, Idx, IdxOffset);

    if (--NumLeadingNonLeafArrDim == 0)
      return Idx;

    PrevTy = IdxTy;
  }

  if (isa<GetElementPtrInst>(*(GI->user_begin())))
    return getTyIndexInFlattenedType_core(
        Func, Idx, cast<GetElementPtrInst>(*(GI->user_begin())),
        AccessMemFormFlatStruct);

  return Idx;
}

Value *
CompositeHandler::getTyIndexInFlattenedType(Type *OTy, GetElementPtrInst *GI,
                                            bool AccessMemFormFlatStruct) {

  Type *Int32Ty = Type::getInt32Ty(Func->getContext());
  OTy = GetRootType(OTy);
  gep_type_iterator IdxTyItr = gep_type_begin(GI);
  assert(OTy == IdxTyItr.getIndexedType() &&
         "GEP starting reference is a non-flattened type");

  return getTyIndexInFlattenedType_core(Func,
                                        ConstantInt::get(Int32Ty, APInt(32, 0)),
                                        GI, AccessMemFormFlatStruct);
}

void CompositeHandler::replaceAllUsesWithUnwrapped(Value *Old, Value *New) {
  assert(Old->getType() != New->getType() && "User stock doRAUW");

  for (auto *User : Old->users())
    for (unsigned i = 0; i < User->getNumOperands(); i++)
      if (User->getOperand(i) == Old)
        User->setOperand(i, New);
}

void CompositeHandler::replaceGetElementPtrChain(Type *Ty, GetElementPtrInst *V,
                                                 ValueSet &LazyRemovalSet,
                                                 ValueVec &AIArr) {
  ValueVec IList;
  if (GetElementPtrInst *lastGEP =
          extractIndices(Ty, V, IList, LazyRemovalSet, isBlastedType(Ty))) {
    Value *Mem = nullptr;
    Type *IndexType = GetElementPtrInst::getIndexedType(
        cast<PointerType>(Ty)->getElementType(), IList);
    Value *AIArrIdx = getTyIndexInFlattenedType(Ty, V, isBlastedType(Ty));

    // For constant indexed GEP directly access the allocation from AIArr
    // (flat structure field allocations) otherwise access has to be made
    // from array of MPT corrosponding to flat structure..
    IRBuilder<> IRB(V);
    if (isBlastedType(Ty)) {
      assert(isa<ConstantInt>(AIArrIdx));
      Mem = AIArr[cast<ConstantInt>(AIArrIdx)->getZExtValue()];
    } else {
      Value *Base = AIArr[0];
      Type *Int32Ty = Type::getInt32Ty(Func->getContext());
      Mem = IRB.CreateGEP(Base,
                          {ConstantInt::get(Int32Ty, APInt(32, 0)), AIArrIdx});
    }

    // Replace all uses of GEP with Mem.
    if (isa<ArrayType>(IndexType)) {
      int NumLeadingNonLeafArrDim = GetNumLeadingNonLeafArrDim(lastGEP);
      bool CreateNewLeafGEP = NumLeadingNonLeafArrDim > 0;
      if (CreateNewLeafGEP) {
        IRB.SetInsertPoint(lastGEP);
        SmallVector<Value *, 4> IdxVec;
        IdxVec.append(lastGEP->idx_begin() + NumLeadingNonLeafArrDim,
                      lastGEP->idx_end());
        Value *NewGEP = IRB.CreateGEP(Mem, IdxVec);
        lastGEP->replaceAllUsesWith(NewGEP);
        InsertIntoSet(LazyRemovalSet, lastGEP);
      } else
        lastGEP->getOperand(1)->replaceAllUsesWith(Mem);
    } else if (isa<StructType>(IndexType)) {
      replaceAllUsesWithUnwrapped(lastGEP, Mem);
      InsertIntoSet(LazyRemovalSet, lastGEP);
    } else {
      lastGEP->replaceAllUsesWith(Mem);
      InsertIntoSet(LazyRemovalSet, lastGEP);
    }
  }
}

void CompositeHandler::copyMemory(Value *Dst, Value *Src, Type *Ty,
                                  ValueVec &ValArr, IRBuilder<> &IRB) {
  assert(objFlatObjArr.find(Dst) != objFlatObjArr.end() &&
         "Destination constituent allocation array missing");
  assert(objFlatObjArr.find(Src) != objFlatObjArr.end() &&
         "Source constituent allocation array missing");
  ValueVec IdxVec;
  IdxVec.push_back(
      ConstantInt::get(Type::getInt32Ty(Func->getContext()), APInt(32, 0)));
  if (!isa<ConstantInt>(Src)) {
    if (isBlastedType(Ty)) {
      Type *BlTy = typeDefs[Ty];
      assert(isa<StructType>(BlTy));
      for (unsigned i = 0; i < cast<StructType>(BlTy)->getNumElements(); i++) {
        Type *ElmTy = cast<StructType>(BlTy)->getElementType(i);
        Value *SrcObj = objFlatObjArr[Src][i];
        Value *DstObj = objFlatObjArr[Dst][i];
        if (isa<SequentialType>(ElmTy)) {
          int NumElms = cast<SequentialType>(ElmTy)->getNumElements();
          CopyCompositeObj(DstObj, SrcObj, ElmTy, NumElms, IdxVec, IRB,
                           Func->getContext());
        } else {
          CopyScalarObj(DstObj, SrcObj, ElmTy, IRB);
        }
      }
    } else {
      // Copy MPT array from Source to Desitination.
      Value *SrcObj = objFlatObjArr[Src][0];
      Value *DstObj = objFlatObjArr[Dst][0];
      Type *SrcObjTy = GetRootType(SrcObj->getType());
      Type *DstObjTy = GetRootType(DstObj->getType());

      assert(isa<ArrayType>(SrcObjTy) && isa<ArrayType>(DstObjTy));
      int NumElms = cast<SequentialType>(SrcObjTy)->getNumElements();
      CopyCompositeObj(DstObj, SrcObj, SrcObjTy, NumElms, IdxVec, IRB,
                       Func->getContext());
    }
  }
}

void CompositeHandler::replaceMemoryIntrinsics(Type *Ty, CallInst *CI,
                                               ValueSet &LazyRemovalSet,
                                               ValueVec &ValArr) {
  ValueVec RemoveInstrs;
  auto GetRootValue = [&](Value *V) -> Value * {
    while (isa<CastInst>(V)) {
      RemoveInstrs.push_back(V);
      V = cast<User>(V)->getOperand(0);
    }
    return V;
  };

  Value *Dst = CI->getOperand(0);
  Value *Src = CI->getOperand(1);
  Value *Size = CI->getOperand(2);

  Dst = GetRootValue(Dst);
  Src = GetRootValue(Src);

  assert(Src->getType() == Dst->getType() && Dst->getType() == Ty &&
         "Memory operation b/w incompatible types");

  Type *RootTy = GetRootType(Ty);
  unsigned TySize =
      Func->getParent()->getDataLayout().getTypeSizeInBits(RootTy) / 8;

  IRBuilder<> IRB(CI);
  if (isa<ConstantInt>(Size) &&
      TySize == cast<ConstantInt>(Size)->getZExtValue()) {
    copyMemory(Dst, Src, Ty, ValArr, IRB);
    RemoveInstrs.push_back(CI);

    // Insert redundant casts for lazy removal.
    for (auto Val : RemoveInstrs)
      InsertIntoSet(LazyRemovalSet, Val);

  } else {
    // TODO
    llvm_unreachable("Variable size memory operation performed.");
  }
}

void CompositeHandler::replaceCastChainAndMemIntrinsics(
    Type *Ty, BitCastInst *V, ValueSet &LazyRemovalSet, ValueVec &ValArr) {
  // TODO: Following are the patterns of interest
  // BitCast + MemIntrinsic(only use)
  // BitCast + GEP(only use)
  // BitCast + Load/Store(only use)
  if (!isa<BitCastInst>(V) || !cast<BitCastInst>(V)->hasOneUse())
    return;

  Value *OnlyUser = *(V->user_begin());
  if (isa<CallInst>(OnlyUser) &&
      cast<CallInst>(OnlyUser)->getCalledFunction()) {
    unsigned ID =
        cast<CallInst>(OnlyUser)->getCalledFunction()->getIntrinsicID();
    assert((ID == Intrinsic::memcpy || ID == Intrinsic::memmove ||
            ID == Intrinsic::memset) &&
           "Unexpected intrinsic");
    replaceMemoryIntrinsics(Ty, cast<CallInst>(OnlyUser), LazyRemovalSet,
                            ValArr);
  }
}

void CompositeHandler::replaceValue(Type *Ty, Value *V, bool RemoveVal,
                                    ValueSet &LazyRemovalSet) {
  ValueVec ValArr;
  assert(objFlatObjArr.find(V) != objFlatObjArr.end());
  ValArr = objFlatObjArr[V];

  for (auto *User : V->users()) {
    if (LazyRemovalSet.find(User) != LazyRemovalSet.end())
      continue;

    if (isa<GetElementPtrInst>(User))
      replaceGetElementPtrChain(Ty, cast<GetElementPtrInst>(User),
                                LazyRemovalSet, ValArr);
    else if (isa<BitCastInst>(User))
      replaceCastChainAndMemIntrinsics(Ty, cast<BitCastInst>(User),
                                       LazyRemovalSet, ValArr);
    else
      llvm_unreachable("Unhandled use of composite alloca use");
  }

  if (RemoveVal)
    InsertIntoSet(LazyRemovalSet, V);
}

void CompositeHandler::doReplacements() {
  ValueSet LazyRemovalSet;
  unsigned ArgFixupCount = 0;
  unsigned AllocaFixupCount = 0;

  for (auto &TyValPair : typeValues) {
    for (Value *V : TyValPair.second) {
      if (isa<AllocaInst>(V)) {
        replaceValue(TyValPair.first, V, true, LazyRemovalSet);
        AllocaFixupCount++;
      } else if (isa<Argument>(V)) {
        replaceValue(TyValPair.first, V, false, LazyRemovalSet);
        ArgFixupCount++;
      } else
        llvm_unreachable("Unhandled instruction for replacement");
    }
  }

  DEBUG(dbgs() << "Number of Arguments Replaced : " << ArgFixupCount << "\n");
  DEBUG(dbgs() << "Number of Alloca Replaced : " << AllocaFixupCount << "\n");

  // TODO: replace removeFromParent with eraseFromParent and verify.
  for (auto *User : LazyRemovalSet)
    cast<Instruction>(User)->removeFromParent();

  if (ArgFixupCount) {
    // TODO : Function cloning ValueMap destructor walks over original
    // function values, deallocating OldArgs (allocated by CloneFunction)
    // makes destructor unhappy.
    // std::allocator<Argument>().deallocate(OldArgs, ArgsCnt);
  }
}

void CompositeHandler::doAllocations() {
  DenseSet<Argument *> WorkListArgs;
  IRBuilder<> IRB(&Func->getEntryBlock().front());

  for (auto &TyValPair : typeValues) {
    for (Value *V : TyValPair.second) {
      if (isa<AllocaInst>(V)) {
        ValueVec ValArr;
        Value *ArraySize = nullptr;
        Type *FlatTy = typeDefs[TyValPair.first];
        FlatTy = GetRootType(FlatTy);
        // If type is blasted type, allocate each field separately over
        // stack since a check for constant indexed references for type has
        // already been done.
        if (isBlastedType(TyValPair.first)) {
          for (unsigned i = 0; i < cast<StructType>(FlatTy)->getNumElements();
               i++) {
            Type *ElmTy = cast<StructType>(FlatTy)->getStructElementType(i);

            if (isa<ArrayType>(ElmTy)) {
              uint64_t ElemCount = cast<ArrayType>(ElmTy)->getNumElements();
              ArraySize = Constant::getIntegerValue(
                  Type::getInt32Ty(Func->getContext()), APInt(32, ElemCount));
              ElmTy = cast<ArrayType>(ElmTy)->getElementType();
            }
            AllocaInst *AI = IRB.CreateAlloca(ElmTy, ArraySize);
            ValArr.push_back(AI);
            IRB.SetInsertPoint(AI);
          }
        } else
          // allocate object of flattened structure/MPT array.
          ValArr.push_back(IRB.CreateAlloca(FlatTy, ArraySize));

        if (objFlatObjArr.find(V) == objFlatObjArr.end())
          objFlatObjArr[V] = ValArr;
      } else if (isa<Argument>(V))
        WorkListArgs.insert(cast<Argument>(V));
    }
  }

  if (WorkListArgs.size() > 0) {
    Argument *OldArgs = BuildAndSetArgsList(Func);
    for (unsigned i = 0; i < Func->arg_size(); i++) {
      Argument *OldArg = OldArgs + i;
      Type *ArgTy = OldArg->getType();
      if (WorkListArgs.find(OldArg) == WorkListArgs.end())
        OldArg->replaceAllUsesWith(Func->arg_begin() + i);
      else {
        ValueVec ValArr;
        assert(typeDefs.find(ArgTy) != typeDefs.end() && typeDefs[ArgTy]);
        Argument *NewArg = new (Func->arg_begin() + i)
            Argument(typeDefs[ArgTy], "", const_cast<Function *>(Func), i);

        CopyArgumentAttributes(OldArg, Func, i);
        ValArr.push_back(NewArg);
        if (objFlatObjArr.find(OldArg) == objFlatObjArr.end())
          objFlatObjArr[OldArg] = ValArr;
      }
    }
  }
}

void CompositeHandler::visitAlloca(AllocaInst &I) {
  visitValue(&I, I.getType());
}

void CompositeHandler::visitCallInst(CallInst &I) {
  for (Value *Arg : I.arg_operands()) {
    visitValue(Arg, Arg->getType());
  }
}

void CompositeHandler::doTypeCollection() {
  // Gather the composite types (structure) to be processed
  // across the function.
  for (Argument &Arg : Func->args())
    visitValue(&Arg, Arg.getType());
  visit(Func);
}

void CompositeHandler::ProcessFunction() {
  doTypeCollection();

  dump();

  flattenTypes();

  dump();

  doAllocations();

  doReplacements();
}
} // namespace llvm
