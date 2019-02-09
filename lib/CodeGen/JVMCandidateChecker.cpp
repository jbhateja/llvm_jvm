//===------------- JVMCandidateChecker.cpp---------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
//
//===----------------------------------------------------------------------===//

#include "llvm/CodeGen/JVMCandidateChecker.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/Analysis/MemorySSA.h"
#include "llvm/CodeGen/JVMCommonUtils.h"
#include "llvm/IR/DiagnosticInfo.h"
#include "llvm/Transforms/Utils/LowerMemIntrinsics.h"

using namespace llvm;
using namespace std;

#define DEBUG_TYPE "candidate-checker"

namespace llvm {

bool CandidateChecker::CheckForGlobalRef(Value *V) {
  assert(V);
  if (isa<ConstantExpr>(V)) {
    bool Res = false;
    Operator *Operand = cast<Operator>(V);
    for (unsigned j = 0; j < Operand->getNumOperands(); j++)
      Res |= CheckForGlobalRef(Operand->getOperand(j));
    return Res;
  }

  if ((isa<GlobalVariable>(V) && cast<GlobalVariable>(V)->isConstant()) ||
      !GlobalVariable::classof(V))
    return false;
  return true;
}

void CandidateChecker::visitInstruction(Instruction &I) {
  // Check operands for type validation.
  for (auto &Opr : I.operands()) {
    if (!CheckValidTypeForTranslation(Opr->getType(), false)) {
      std::string error = "Illegal operand type for instruction transition";
      report_note(error, false);
      I.dump();
      visitStatus = false;
      break;
    }
  }
}

void CandidateChecker::visitTerminatorInst(Instruction &I) {
  if (I.isExceptionalTerminator()) {
    report_note("Exception terminator instructions currently not supported",
                false);
    visitStatus = false;
  } else if (dyn_cast<IndirectBrInst>(&I)) {
    report_note("Indirect branch instruction currently not supported", false);
    visitStatus = false;
  }
}

void CandidateChecker::visitLoad(LoadInst &I) {
  if (CheckForGlobalRef(I.getPointerOperand())) {
    std::string error = "Load from global memory ";
    error += I.getName();
    error += " not supported.";
    report_note(error, false);
    visitStatus = false;
  }
}

static bool CheckIfAllUsesAreConstantReferencesToSource(Value *I) {
  bool RetVal = true;
  if (isa<GetElementPtrInst>(I)) {
    if (!cast<GetElementPtrInst>(I)->hasAllConstantIndices())
      return false;
    for (auto *Users : I->users()) {
      RetVal &= CheckIfAllUsesAreConstantReferencesToSource(Users);
    }
  } else if (isa<LoadInst>(I) || isa<StoreInst>(I))
    return true;
  else if (isa<BitCastInst>(I)) {
    for (auto *Users : I->users()) {
      RetVal &= CheckIfAllUsesAreConstantReferencesToSource(Users);
    }
  }
  return RetVal;
}

bool CandidateChecker::CheckTypeCompatibilityForBitCast(Type *SrcTy,
                                                        Type *DstTy) {
  const DataLayout &DL = Func->getParent()->getDataLayout();

  if ((isa<PointerType>(SrcTy) && !isa<PointerType>(DstTy)) ||
      (!isa<PointerType>(SrcTy) && isa<PointerType>(DstTy)))
    return false;

  if (isa<PointerType>(SrcTy))
    SrcTy = cast<PointerType>(SrcTy)->getElementType();
  if (isa<PointerType>(DstTy))
    DstTy = cast<PointerType>(DstTy)->getElementType();

  uint64_t SrcTySz = DL.getTypeSizeInBits(SrcTy);
  uint64_t DstTySz = DL.getTypeSizeInBits(DstTy);

  // Source type should be at least as big as destination type
  // to accommodate destination reference into it, since we
  // transform all the destination references to references
  // w.r.t source (root).
  return DstTySz <= SrcTySz;
}

// BitCast is a no-op cast, used mainly for reinterpretation.
void CandidateChecker::visitBitCastInst(BitCastInst &I) {
  LLVMContext &Cxt = Func->getContext();
  Value *Root = GetRootObject2(I.getOperand(0), Cxt);
  if (!Root) {
    visitStatus = false;
  }
  Type *SrcTy = Root->getType();
  Type *DstTy = I.getDestTy();
  if (!CheckTypeCompatibilityForBitCast(SrcTy, DstTy)) {
    std::string error = "Incompatible source and destination type for cast";
    report_note(error, false);
    visitStatus = false;
  }

  // For structure type source, offset computed from destination type
  // must be a constant, since fields of structures are at constant offsets
  // from the base.
  Type *RootTy = Root->getType();
  if (CheckIfLeafElementIsStructureType(RootTy) &&
      !CheckIfAllUsesAreConstantReferencesToSource(&I)) {
    std::string error =
        "Source memory of structure type cannot be indexed by variable offsets";
    report_note(error, false);
    visitStatus = false;
  }
}

int CandidateChecker::GetNumArrayDimensions(Type *Ty) {
  int Dim = 0;
  while (isa<ArrayType>(Ty)) {
    Ty = cast<ArrayType>(Ty)->getElementType();
    Dim++;
  }
  return Dim;
}

void CandidateChecker::visitAlloca(AllocaInst &I) {
  Type *Ty = I.getAllocatedType();
  Value *Sz = I.getArraySize();
  if (isa<ArrayType>(Ty)) {
    if ((isa<ConstantInt>(Sz) && cast<ConstantInt>(Sz)->getZExtValue() > 1) ||
        !isa<ConstantInt>(Sz) || GetNumArrayDimensions(Ty) > 1) {
      std::string error = "Multi-dimensional arrays ";
      error += I.getName();
      error += " not supported.";
      report_note(error, false);
      visitStatus = false;
    }
  }
  if (!CheckValidTypeForTranslation(Ty, false)) {
    std::string error = "Illegal type for translation.";
    report_note(error, false);
    visitStatus = false;
  }
}

void CandidateChecker::visitStore(StoreInst &I) {
  if (CheckForGlobalRef(I.getPointerOperand())) {
    std::string error = "Store from global memory ";
    error += I.getName();
    error += " not supported.";
    report_note(error, false);
    visitStatus = false;
  }
}

void CandidateChecker::visitGetElementPtrInst(GetElementPtrInst &I) {
  if (CheckForGlobalRef(I.getPointerOperand())) {
    std::string error = "Access to global memory ";
    error += I.getName();
    error += " not supported.";
    report_note(error, false);
    visitStatus = false;
  }

  // Avoid out of bound acceses made to a scalar allocation.
  bool HasBitCastInChain;
  LLVMContext &Cxt = Func->getContext();
  const DataLayout &DL = Func->getParent()->getDataLayout();
  Value *Offset = ConstantInt::get(Type::getInt32Ty(Cxt), 0);

  Value *Mem = GetRootObject(&I, &Offset, HasBitCastInChain);
  Type *MemType = GetRootType(Mem->getType());
  unsigned MemSzInBits = DL.getTypeSizeInBits(GetRootType(MemType));
  unsigned AccessSzInBits = DL.getTypeSizeInBits(GetRootType(I.getType()));

  if (!isa<CompositeType>(MemType) && !HasBitCastInChain &&
      (!isa<ConstantInt>(Offset) ||
       ((cast<ConstantInt>(Offset)->getZExtValue() + AccessSzInBits) >
        MemSzInBits))) {
    std::string error = "Out of memory access made to scalar object by ";
    error += I.getName();
    error += " not supported.";
    report_note(error, false);
    visitStatus = false;
  }
}

// TODO: Existence of library call, non-inlinable function.
void CandidateChecker::visitCall(CallInst &I) {
  bool IsMemset = false;
  bool IsMemMove = false;
  LLVMContext &Cxt = Func->getContext();
  auto SupportedIntrinsics = [&](unsigned ID) -> bool {
    switch (ID) {
    default:
      return false;
    case Intrinsic::memset:
      IsMemset = true;
      break;
    case Intrinsic::memmove:
      IsMemMove = true;
      break;
    case Intrinsic::memcpy:
      break;
    }
    return true;
  };

  auto CheckForValidMoves = [&](MemIntrinsic *MI) -> bool {
    Value *Dst = MI->getRawDest();
    Value *Src = GetMemorySrc(MI);
    bool IsDstOrSrcPartOfStruct =
        (CheckIfLeafElementIsStructureType(Dst->getType()) &
         CheckIfValuePartOfStructure(Dst)) ||
        (CheckIfLeafElementIsStructureType(Src->getType()) &
         CheckIfValuePartOfStructure(Src));

    // Variable length memory operations involving structures
    // makes a function non-translatable since structure fields
    // cannot be deterministically determined with variable
    // field offset.
    if (IsDstOrSrcPartOfStruct && !isa<ConstantInt>(MI->getLength()))
      return false;

    if (IsDstOrSrcPartOfStruct && IsMemMove)
      return false;

    // Memmove will be transformed to memcpy if source and destination offsets
    // from root object are same, else candidate is non-translatable.
    if (IsMemMove) {
      Value *SrcOffset = GetOffsetFromRootValue(Cxt, Src);
      Value *DstOffset = GetOffsetFromRootValue(Cxt, Dst);
      if (!isa<ConstantInt>(SrcOffset) || !isa<ConstantInt>(DstOffset) ||
          cast<ConstantInt>(SrcOffset)->getZExtValue() !=
              cast<ConstantInt>(DstOffset)->getZExtValue())
        return false;
    }

    return true;
  };

  if (Function *F = I.getCalledFunction()) {
    if (!F->isDeclaration()) {
      visitStatus = false;
      return;
    } else if (!F->getIntrinsicID()) {
      visitStatus = false;
      std::string error = "Call to non-intrinsic ";
      error += I.getName();
      error += " not supported.";
      report_note(error, false);
      visitStatus = false;
    } else if (!SupportedIntrinsics(F->getIntrinsicID()) ||
               !CheckForValidMoves(cast<MemIntrinsic>(&I))) {
      std::string error = "Call to intrinsic ";
      error += I.getName();
      error += " not supported.";
      report_note(error, false);
      visitStatus = false;
    }
  }
}

bool CandidateChecker::CheckValidTypeForTranslation(Type *Ty, bool ByVal) {
  bool ret = true;
  // Given a struct type, recursively traverse the elements.
  if (StructType *STy = dyn_cast<StructType>(Ty)) {
    for (StructType::element_iterator EB = STy->element_begin(), EI = EB,
                                      EE = STy->element_end();
         EI != EE; ++EI) {
      ret &= CheckValidTypeForTranslation(*EI);
    }
  }
  // Given an array type, traverse element type.
  else if (ArrayType *ATy = dyn_cast<ArrayType>(Ty)) {
    Type *EltTy = ATy->getElementType();
    ret &= CheckValidTypeForTranslation(EltTy);
  } else if (dyn_cast<PointerType>(Ty) && ByVal) {
    ret &=
        CheckValidTypeForTranslation(cast<PointerType>(Ty)->getElementType());
  } else if (dyn_cast<PointerType>(Ty)) {
    report_note("Pointer currently is an invalid type for translation", false);
    return false;
  } else if (dyn_cast<VectorType>(Ty)) {
    // Vector needs to be disintegrated into its constituent elements.
    report_note("Vectortype is currently is an invalid type for translation",
                false);
    return false;
  } else if (isa<IntegerType>(Ty) && Ty->getScalarSizeInBits() > 1 &&
             !cast<IntegerType>(Ty)->isPowerOf2ByteWidth()) {
    report_note("Integral types with sizes non-power of 2 not supported for "
                "translation",
                false);
    return false;
  } else if (Ty->isFloatingPointTy()) {
    report_note("Floating point type is an invalid type for translation",
                false);
    return false;
  }
  return ret;
}

bool CandidateChecker::CheckFuncSignature() {
  int ArgsSize = 0;
  DenseSet<Type *> Set;
  Argument *SRetArg = nullptr;
  const DataLayout &DL = Func->getParent()->getDataLayout();

  for (auto Argument = Func->arg_begin(); Argument != Func->arg_end();
       Argument++) {
    if (Argument->hasJVMArgBufAttr())
      continue;

    if (Argument->hasStructRetAttr()) {
      assert(SRetArg == nullptr && "Multiple return objects found");
      SRetArg = Argument;
      continue;
    }
    Type *ArgType = Argument->getType();
    if (!CheckValidTypeForTranslation(ArgType, Argument->hasByValAttr())) {
      std::string error = Argument->getName();
      error += " has type not valid for translation";
      report_note(error, false);
    }
    if (isa<PointerType>(ArgType) && Argument->hasByValAttr())
      ArgType = cast<PointerType>(ArgType)->getElementType();
    ArgsSize += DL.getTypeSizeInBits(ArgType);
  }

  // Check for hard constraints over argument sizes.
  if ((ArgsSize / 8) >= IOBUFSIZE) {
    report_note("Cumulative arguments size exceeds IOBUF limits.");
    return false;
  }

  // Check return value size.
  if (!Func->getReturnType()->isVoidTy() || SRetArg) {
    Type *RetTy = SRetArg
                      ? cast<PointerType>(SRetArg->getType())->getElementType()
                      : Func->getReturnType();
    if (!CheckValidTypeForTranslation(RetTy))
      report_note("Return type not valid for translation");

    int ReturnSize = DL.getTypeSizeInBits(RetTy);
    if ((ReturnSize / 8) >= IOBUFSIZE) {
      report_note("Return arguments size exceeds IOBUF limits.");
      return false;
    }
  }

  return true;
}

Value *CandidateChecker::GetModifiedMemoryLoc(Value *II) {
  LLVMContext &Cxt = Func->getContext();
  if (isa<StoreInst>(II))
    return cast<StoreInst>(II)->getPointerOperand();
  else if (isa<CallInst>(II) &&
           cast<CallInst>(II)->getCalledFunction()->getIntrinsicID()) {
    CallInst *CI = cast<CallInst>(II);
    unsigned IID = CI->getCalledFunction()->getIntrinsicID();
    assert(IID == Intrinsic::memcpy || IID == Intrinsic::memset ||
           IID == Intrinsic::memmove);
    return GetRootObject2(CI->getOperand(0), Cxt);
  } else
    llvm_unreachable(
        "Unhandled instruction type for memory operand extraction");
  return nullptr;
}

bool CandidateChecker::CheckDefList(LoadInst *LI, MemorySSA &MSSA,
                                    const MemoryAccess *MA,
                                    bool CheckBlockDefs) {
  bool Res = true;
  LLVMContext &Cxt = Func->getContext();
  if (isa<MemoryDef>(MA)) {
    if (!cast<MemoryUseOrDef>(MA)->getMemoryInst()) {
      Value *MemAcc = GetRootObject2(LI->getPointerOperand(), Cxt);
      if (isa<GlobalVariable>(MemAcc))
        return true;
      else
        return false;
    }
    Value *ModMem =
        GetModifiedMemoryLoc(cast<MemoryUseOrDef>(MA)->getMemoryInst());

    Value *MemWrite = GetRootObject2(ModMem, Cxt);
    Value *MemRead = GetRootObject2(LI->getPointerOperand(), Cxt);
    Type *MemWriteTy = GetRootType(MemWrite->getType());
    Type *MemReadTy = GetRootType(MemRead->getType());

    // Objects of composite types are initialized by constructor invocation.
    if (isa<CompositeType>(MemReadTy) || isa<CompositeType>(MemWriteTy))
      return true;

    Res = MemWrite == MemRead;
  } else if (isa<MemoryPhi>(MA)) {
    for (unsigned i = 0; i < cast<MemoryPhi>(MA)->getNumIncomingValues(); i++)
      Res &= CheckDefList(LI, MSSA, cast<MemoryPhi>(MA)->getIncomingValue(i),
                          true);
  }

  // Return if valid store found.
  if (Res)
    return true;

  // Check for store corresponding to load in remaining block deflist.
  if (CheckBlockDefs) {
    for (auto &Def : *MSSA.getBlockDefs(MA->getBlock())) {
      if (&Def == MA)
        continue;
      if (CheckDefList(LI, MSSA, &Def, false))
        return true;
    }
  }
  return false;
}

bool CandidateChecker::CheckInitializedMemoryRead(MemorySSA &MSSA,
                                                  LoadInst *LI) {
  LLVMContext &Cxt = Func->getContext();
  MemoryAccess *MA = MSSA.getMemoryAccess(LI);
  MA = cast<MemoryUseOrDef>(MA)->getDefiningAccess();

  // Memory load of/from composite type is initialized by
  // constructor at the time of allocation.
  Value *LoadSrc = GetRootObject2(LI->getPointerOperand(), Cxt);
  Type *LoadSrcTy = GetRootType(LoadSrc->getType());
  if (isa<CompositeType>(LoadSrcTy))
    return true;

  return CheckDefList(LI, MSSA, MA, false);
}

bool CandidateChecker::CheckInitializedRetVal(PHINode *PHI) {
  bool RetVal = true;
  for (auto &Val : PHI->incoming_values())
    if (isa<UndefValue>(Val))
      return false;
    else if (isa<PHINode>(Val))
      RetVal &= CheckInitializedRetVal(cast<PHINode>(Val));
  return RetVal;
}

void CandidateChecker::CheckUninitializedMemoryReads(MemorySSA &MSSA) {
  SmallVector<LoadInst *, 8> LoadList;
  SmallVector<ReturnInst *, 8> RetList;

  for (auto &BB : *Func)
    for (auto &II : BB)
      if (isa<LoadInst>(&II))
        LoadList.push_back(cast<LoadInst>(&II));
      else if (isa<ReturnInst>(&II))
        RetList.push_back(cast<ReturnInst>(&II));

  // Check initialization store for each load.
  for (auto *II : LoadList) {
    if (!CheckInitializedMemoryRead(MSSA, II)) {
      report_note("Uninitialized memory read encountered");
      break;
    }
  }

  // Check return value
  for (auto *II : RetList) {
    Value *RetVal = II->getReturnValue();
    if (RetVal && isa<PHINode>(RetVal)) {
      if (!CheckInitializedRetVal(cast<PHINode>(RetVal))) {
        report_note("Uninitialized return value encountered");
        break;
      }
    }
  }
}

bool CandidateChecker::IsCandidateForTranslation(MemorySSA &MSSA) {
  const DataLayout &DL = Func->getParent()->getDataLayout();
  TargetTransformInfo TTI(DL);

  if (!CheckFuncSignature())
    return false;

  (void)visit(*Func);

  if (!visitStatus)
    return false;

  CheckUninitializedMemoryReads(MSSA);

  return visitStatus;
}

template <typename T>
void CandidateChecker::report_note(T &&error, bool Suppress) {
  LLVMContext &Cxt = Func->getContext();
  if (!Suppress)
    Cxt.emitError(error, DiagnosticSeverity::DS_Note);
  visitStatus = false;
}

void CandidateChecker::setFunction(Function &F) {
  Func = &F;
  visitStatus = true;
}
} // namespace llvm
