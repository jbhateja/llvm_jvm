//===------------- JVMIRDecorator.cpp -------------------------------------===//
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

#include "llvm-c/Types.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/FoldingSet.h"
#include "llvm/ADT/PointerIntPair.h"
#include "llvm/ADT/PostOrderIterator.h"
#include "llvm/ADT/SmallSet.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Analysis/CodeMetrics.h"
#include "llvm/Analysis/ConstantFolding.h"
#include "llvm/Analysis/TargetTransformInfo.h"
#include "llvm/CodeGen/JVMCommonUtils.h"
#include "llvm/CodeGen/Passes.h"
#include "llvm/IR/CFG.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/GetElementPtrTypeIterator.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/InstVisitor.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/PassManager.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/Error.h"
#include "llvm/Transforms/Utils/BasicBlockUtils.h"
#include "llvm/Transforms/Utils/Cloning.h"
#include "llvm/Transforms/Utils/LoopUtils.h"

#include "JVMCastHandler.h"
#include "JVMCompositeHandler.h"

#include "llvm/Pass.h"
#include "llvm/Support/raw_ostream.h"

#define DEBUG_TYPE "jvm-ir-decorator"

using namespace llvm;

STATISTIC(FuncDecoratedCounter, "Counts number of functions greeted");

namespace {

class JVMIRDecorator : public FunctionPass {
public:
  using ValueMap = DenseMap<Value *, Value *>;
  using GlobFieldOffsetMap = DenseMap<Value *, unsigned>;

  static char ID; // Pass identification, replacement for typeid
  JVMIRDecorator() : FunctionPass(ID) {}

  bool runOnFunction(Function &F) override {
    Func = &F;
    DT = &getAnalysis<DominatorTreeWrapperPass>().getDomTree();

    return ProcessFunction();
  }

  bool ProcessFunction();

  /// JVM-Server = llc -jvm-server + Jasmin Assembler.
  /// JVM-Client = llc -jvm-client + Javac compiler.

  /// In optimization mode (-jvm-server) llc's JVM backend will be
  /// directly generating code in Jasmin assembly which will cover
  /// entire functionality of following routines.

  void HandleCompositeTypes(Function *F);
  void HandleBitCastOperation(Function *F);
  void EmitArgumentInitializationWrappers();

  /// We don't modify the program, so we preserve all analyses.
  void getAnalysisUsage(AnalysisUsage &AU) const override {
    getLoopAnalysisUsage(AU);
  }

private:
  void LinearizeGEPChains(Function *F);
  void LinearizeGEP(IRBuilder<> &IRB, Value *Ptr, Value *GEP, bool SoC,
                    SmallVectorImpl<Value *> &Indices);

  void UniquifyGEPOperands(Function *F);
  void UniquifyGEPChain(Value *GEP, Value *GEPclone);

  void FixupCompositeIndexes(Function *F);

  void LowerSELECT(Function *F);
  void LowerSELECTOp(Value *Select);

  void ReplaceConstantGlobsRefs(GlobalVariable *GV, GlobalVariable *GCStruct,
                                GlobFieldOffsetMap &GlobOffsetMap);
  void HandleGlobalConstants(Function *F);

  void ConstantFoldInstructions(Function *F);

  void DropAllReferences(User *V);

  void PerformIRFixups(Function *F);

  void FixupICmp(Instruction *I);

  DominatorTree *DT;
  CastHandler BitCastXForm;
  CompositeHandler TyDefCol;
  Function *Func;
};

void JVMIRDecorator::EmitArgumentInitializationWrappers() {
  // 1/ It will generate a clone of translation candidate which will have
  //    just one argument which is a byte array of IOBUF.
  // 2/ Clone will be modified internally such that all the references
  //    to arguments are replaced by references from within IOBUF.
  // 3/ It will generate a stub which will have call to VM (dongle)
  // initialization
  //    along with call to invoke VM to evaluate clone.
  LLVM_DEBUG(dbgs() << "Emitting wrappers");
}

void JVMIRDecorator::HandleCompositeTypes(Function *F) {
  TyDefCol.setFunction(*F);
  TyDefCol.ProcessFunction();
}

void JVMIRDecorator::HandleBitCastOperation(Function *F) {
  BitCastXForm.setFunction(*F);
  BitCastXForm.ProcessFunction();
}

void JVMIRDecorator::UniquifyGEPChain(Value *GEP, Value *GEPclone) {
  Value *GEPPtr = cast<GetElementPtrInst>(GEP)->getPointerOperand();
  Value *GEPclonePtr = cast<GetElementPtrInst>(GEPclone)->getPointerOperand();
  if (isa<GetElementPtrInst>(GEPPtr)) {
    SmallVector<Value *, 8> Indices;
    for (auto &Index : cast<GetElementPtrInst>(GEPPtr)->indices())
      Indices.push_back(Index);

    IRBuilder<> IRB(cast<Instruction>(GEPclone));
    Value *GEPPtrClone = IRB.CreateGEP(
        cast<GetElementPtrInst>(GEPPtr)->getPointerOperand(), Indices);
    cast<Instruction>(GEPclone)->replaceUsesOfWith(GEPclonePtr, GEPPtrClone);

    UniquifyGEPChain(GEPPtr, GEPPtrClone);
  }
}

void JVMIRDecorator::LinearizeGEP(IRBuilder<> &IRB, Value *Ptr, Value *GEP,
                                  bool SoC, SmallVectorImpl<Value *> &Indices) {
  unsigned i = 0;
  unsigned init = 0;
  Value *LastIndex = nullptr;
  const DataLayout &DL = Func->getParent()->getDataLayout();
  Value *GEPPtr = cast<GetElementPtrInst>(GEP)->getPointerOperand();
  Type *GEPPtrRootTy = GetRootType(GEPPtr->getType());

  if (!isa<CompositeType>(GEPPtrRootTy)) {
    // NewIndex = Pop_Last_index() + GEP_Index;
    assert(cast<GetElementPtrInst>(GEP)->getNumIndices() == 1);
    LastIndex = Indices.back();
    Indices.pop_back();
    Value *Index = cast<GetElementPtrInst>(GEP)->getOperand(1);
    Index = GetTypeNormalizedValue(DL, IRB, Index, LastIndex->getType());
    Value *NextIndex = IRB.CreateAdd(LastIndex, Index);
    Indices.push_back(NextIndex);
  } else {
    init = SoC ? 0 : 1;
    for (i = init; i < cast<GetElementPtrInst>(GEP)->getNumIndices(); i++)
      Indices.push_back(cast<GetElementPtrInst>(GEP)->getOperand(i + 1));
  }

  SmallVector<std::pair<Instruction *, Value *>, 8> UserGEPClonePairVec;
  for (auto *User : GEP->users()) {
    IRB.SetInsertPoint(cast<Instruction>(User));
    if (isa<GetElementPtrInst>(User))
      LinearizeGEP(IRB, Ptr, User, false, Indices);
    else {
      Value *NewMergeGEP = IRB.CreateGEP(Ptr, Indices);
      UserGEPClonePairVec.push_back(
          std::make_pair(cast<Instruction>(User), NewMergeGEP));
    }
  }
  for (auto UserClonePair : UserGEPClonePairVec)
    UserClonePair.first->replaceUsesOfWith(GEP, UserClonePair.second);

  for (i = init; i < cast<GetElementPtrInst>(GEP)->getNumIndices(); i++)
    Indices.pop_back();

  if (LastIndex)
    Indices.push_back(LastIndex);
}

void JVMIRDecorator::LinearizeGEPChains(Function *F) {
  IRBuilder<> IRB(&*F->begin());
  SmallVector<Value *, 8> WorkList;
  // Merge GEP chain into a single GEP instruction, this is useful
  // specially when GEP chain is a composite reference, uniquification
  // of merged GEP instruction later will ensure that each user of GEP
  // get complete chain (merged GEP).
  for (auto &BB : *F)
    for (auto &II : BB)
      if (isa<GetElementPtrInst>(&II)) {
        Value *Ptr = cast<GetElementPtrInst>(&II)->getPointerOperand();
        // Collect only GEPs at the start of GEP chain.
        bool IsInGEPChain = isa<GetElementPtrInst>(Ptr);
        for (auto *User : II.users()) {
          if (!IsInGEPChain && isa<GetElementPtrInst>(User)) {
            WorkList.push_back(&II);
            break;
          }
        }
      }

  for (auto *GEP : WorkList) {
    SmallVector<Value *, 8> Indices;
    Value *Ptr = cast<GetElementPtrInst>(GEP)->getPointerOperand();
    LinearizeGEP(IRB, Ptr, GEP, true, Indices);
  }
}

void JVMIRDecorator::UniquifyGEPOperands(Function *F) {
  SmallVector<Value *, 8> WorkList;
  // Uniquification offers duplicating shared operands(GEPs)
  // and also taking operands close to their users.
  for (auto &BB : *F)
    for (auto &II : BB)
      if (isa<GetElementPtrInst>(&II)) {
        for (auto *User : II.users()) {
          // Collect only GEPs at the end of GEP chain.
          if (!isa<GetElementPtrInst>(User)) {
            WorkList.push_back(&II);
            break;
          }
        }
      }

  for (auto *II : WorkList) {
    GetElementPtrInst *GEP = cast<GetElementPtrInst>(II);
    if (GEP->use_empty())
      continue;

    SmallVector<Value *, 8> Indices;
    for (auto &Index : GEP->indices())
      Indices.push_back(Index);

    Value *GEPclone = nullptr;
    SmallVector<std::pair<Instruction *, Value *>, 8> UserGEPClonePairVec;
    for (auto &Use : GEP->uses()) {
      Instruction *UI = cast<Instruction>(Use.getUser());
      IRBuilder<> IRB(UI);
      GEPclone = IRB.CreateGEP(GEP->getPointerOperand(), Indices);
      UserGEPClonePairVec.push_back(std::make_pair(UI, GEPclone));
    }
    for (auto UserClonePair : UserGEPClonePairVec) {
      UserClonePair.first->replaceUsesOfWith(GEP, UserClonePair.second);
      // Clone entire GEP chain
      UniquifyGEPChain(GEP, UserClonePair.second);
    }
  }
}

void JVMIRDecorator::FixupCompositeIndexes(Function *F) {
  SmallVector<Value *, 8> WorkList;
  using IndexKind = enum { SCALAR_REF, ARRAY_REF, STRUCT_REF };
  LLVMContext &Cxt = Func->getContext();
  const DataLayout &DL = F->getParent()->getDataLayout();
  for (auto &BB : *F) {
    for (auto &II : BB) {
      if (isa<GetElementPtrInst>(&II)) {
        bool Change = false;
        SmallVector<Value *, 8> Indices;
        IRBuilder<> IRB(cast<Instruction>(&II));
        IndexKind NextIndex = SCALAR_REF;
        for (gep_type_iterator GTI = gep_type_begin(&II), E = gep_type_end(&II);
             GTI != E; ++GTI) {
          const Value *Idx = GTI.getOperand();
          Type *IdxTy = GTI.getIndexedType();

          if (NextIndex == ARRAY_REF) {
            Value *NewIdx = GetTypeNormalizedValue(
                DL, IRB, const_cast<Value *>(Idx), Type::getInt32Ty(Cxt));
            Change = Idx != NewIdx;
            Idx = NewIdx;
          }

          Indices.push_back(const_cast<Value *>(Idx));

          if (isa<StructType>(IdxTy))
            NextIndex = STRUCT_REF;
          else if (isa<ArrayType>(IdxTy))
            NextIndex = ARRAY_REF;
          else
            NextIndex = SCALAR_REF;
        }

        if (Change) {
          Value *NewII = IRB.CreateGEP(
              cast<GetElementPtrInst>(&II)->getPointerOperand(), Indices);
          II.replaceAllUsesWith(NewII);
        }
      }
    }
  }
}

void JVMIRDecorator::LowerSELECTOp(Value *Op) {

  LLVMContext &Cxt = Func->getContext();
  IRBuilder<> IRB(&(*Func->getEntryBlock().begin()));
  SelectInst *Select = cast<SelectInst>(Op);

  Value *Cond = Select->getCondition();
  Value *TrueVal = Select->getTrueValue();
  Value *FalseVal = Select->getFalseValue();
  BasicBlock::iterator SplitIt = Select->getIterator();
  BasicBlock *EndBlk = SplitBlock(Select->getParent(), &*++SplitIt, DT);
  Instruction *OldTI = cast<Instruction>(*EndBlk->user_begin());
  BasicBlock *NewBlk = OldTI->getParent();

  BasicBlock *TrueBlk =
      BasicBlock::Create(Cxt, NewBlk->getName() + ".trueblk", Func);
  TrueBlk->moveAfter(NewBlk);
  BasicBlock *FalseBlk =
      BasicBlock::Create(Cxt, NewBlk->getName() + ".falseblk", Func);
  FalseBlk->moveAfter(TrueBlk);

  IRB.SetInsertPoint(OldTI);
  IRB.CreateCondBr(Cond, TrueBlk, FalseBlk);
  OldTI->eraseFromParent();

  IRB.SetInsertPoint(TrueBlk);
  IRB.CreateBr(EndBlk);

  IRB.SetInsertPoint(FalseBlk);
  IRB.CreateBr(EndBlk);

  IRB.SetInsertPoint(EndBlk->getFirstNonPHI());
  PHINode *PHI = IRB.CreatePHI(Select->getType(), 2);
  PHI->addIncoming(TrueVal, TrueBlk);
  PHI->addIncoming(FalseVal, FalseBlk);
  Select->replaceAllUsesWith(PHI);
}

void JVMIRDecorator::DropAllReferences(User *V) {
  for (Use &U : V->operands()) {
    if (isa<ConstantExpr>(U.get()))
      DropAllReferences(cast<User>(U.get()));
    U.set(nullptr);
  }
}

void JVMIRDecorator::LowerSELECT(Function *F) {
  SmallVector<Value *, 8> WorkList;
  for (auto &BB : *F)
    for (auto &II : BB)
      if (isa<SelectInst>(&II))
        WorkList.push_back(&II);

  for (auto *II : WorkList)
    LowerSELECTOp(II);
}

void JVMIRDecorator::ReplaceConstantGlobsRefs(
    GlobalVariable *GV, GlobalVariable *GCStruct,
    GlobFieldOffsetMap &GlobOffsetMap) {
  LLVMContext &Cxt = Func->getContext();
  const DataLayout &DL = Func->getParent()->getDataLayout();
  Type *TargetIdxTy = GetTargetIndexType(DL, Cxt);
  SmallVector<Instruction *, 4> RemoveList;
  SmallVector<std::pair<Instruction *, Value *>, 8> UserGEPClonePairVec;
  for (auto Use = GV->use_begin(); Use != GV->use_end(); Use++) {
    SmallVector<Value *, 4> Indices;
    unsigned FldIndex = GlobOffsetMap[GV];
    Indices.push_back(ConstantInt::get(TargetIdxTy, 0));
    Indices.push_back(ConstantInt::get(TargetIdxTy, FldIndex));

    Value *User = GetOperatorInstr(Use->getUser());
    // Hack to mask ConstantFolding bug which creates an
    // intermediate constant GEP to normalize indexes w.r.t
    // source pointer type which in process causes multiple
    // uses to be added to UseList of source pointer (global),
    // Since actual folding happens after index normalization
    // thus if folding happens then a constant is returned back
    // and there is no way to drop references added to UseList
    // by intermediate GEP.
    if (!User)
      continue;

    IRBuilder<> IRB(cast<Instruction>(User));
    if (isa<GetElementPtrInst>(User)) {
      GetElementPtrInst *G = cast<GetElementPtrInst>(User);
      auto IdxItr = G->idx_begin();

      // Skip 0th dereferencing index for Array/Structs.
      if (isa<CompositeType>(GetRootType(G->getPointerOperand()->getType())))
        IdxItr++;
      for (; IdxItr != G->idx_end(); IdxItr++)
        Indices.push_back(*IdxItr);

      Value *NewGEP = IRB.CreateGEP(GCStruct, Indices);
      G->replaceAllUsesWith(NewGEP);
      RemoveList.push_back(G);
    } else {
      Value *NewGEP = IRB.CreateGEP(GCStruct, Indices);
      UserGEPClonePairVec.push_back(
          std::make_pair(cast<Instruction>(*Use), NewGEP));
      RemoveList.push_back(cast<Instruction>(User));
    }
  }
  for (auto UserClonePair : UserGEPClonePairVec)
    UserClonePair.first->replaceUsesOfWith(GV, UserClonePair.second);

  for (auto Inst : RemoveList) {
    DropAllReferences(Inst);
    Inst->eraseFromParent();
  }
}

void JVMIRDecorator::HandleGlobalConstants(Function *F) {
  // Pull the constant dependencies into a new structure.
  Module *M = F->getParent();
  if (M->global_empty())
    return;

  unsigned FieldIndex = 0;
  SmallVector<Type *, 8> TypeList;
  SmallVector<Constant *, 8> InitVals;
  GlobFieldOffsetMap GlobOffsetMap;
  for (auto &Glob : M->globals()) {
    TypeList.push_back(GetRootType(Glob.getType()));
    InitVals.push_back(Glob.getInitializer());
    GlobOffsetMap[&Glob] = FieldIndex++;
  }

  StructType *GCStructTy = StructType::create(TypeList);
  GlobalVariable *GCStructObj =
      new GlobalVariable(*M, GCStructTy, true, GlobalValue::PrivateLinkage,
                         nullptr, "ConstantInitStruct");
  Constant *GCStructInit = ConstantStruct::get(GCStructTy, InitVals);
  GCStructObj->setInitializer(GCStructInit);

  // Replace the uses of constant dependencies with field references
  // from newly create structure.
  SmallVector<GlobalVariable *, 4> RemoveList;
  for (auto &Glob : M->globals())
    if (&Glob != GCStructObj) {
      ReplaceConstantGlobsRefs(&Glob, GCStructObj, GlobOffsetMap);
      RemoveList.push_back(&Glob);
    }

  // Remove all other constant globals apart from ConstantInitStruct
  // from module.
  for (auto &Glob : RemoveList)
    Glob->eraseFromParent();

  // Put an attribute over function for marking it as instance method
  // Offset allocator will start allocating offsets from 1 in this case.
  // and assign 0th offset to GCStructObj.
  F->addFnAttr(Attribute::JVMInstMethod);
}

void JVMIRDecorator::ConstantFoldInstructions(Function *F) {
  SmallVector<Instruction *, 8> DelList;
  const DataLayout &DL = Func->getParent()->getDataLayout();
  for (auto &BB : *F)
    for (auto &II : BB)
      if (Value *Const = ConstantFoldInstruction(&II, DL, nullptr)) {
        II.replaceAllUsesWith(Const);
        DelList.push_back(&II);
      }

  for (auto Inst : DelList) {
    DropAllReferences(Inst);
    Inst->removeFromParent();
  }
}

void JVMIRDecorator::FixupICmp(Instruction *I) {
  SmallVector<std::pair<Instruction *, Value *>, 8> UserGEPClonePairVec;
  for (auto User : I->users()) {
    if (!isa<BranchInst>(User) && !isa<SelectInst>(User)) {
      IRBuilder<> IRB(GetOperatorInstr(User));
      Value *Select = IRB.CreateSelect(I, IRB.getInt32(1), IRB.getInt32(0));
      Value *Trunc = IRB.CreateTrunc(Select, IRB.getInt1Ty());
      UserGEPClonePairVec.push_back(
          std::make_pair(cast<Instruction>(User), Trunc));
    }
  }

  for (auto UserClonePair : UserGEPClonePairVec)
    UserClonePair.first->replaceUsesOfWith(I, UserClonePair.second);
}

void JVMIRDecorator::PerformIRFixups(Function *F) {
  SmallVector<Instruction *, 8> WorkList;
  auto IsFixupCandidate = [](unsigned Opcode) {
    switch (Opcode) {
    default:
      return false;
    case Instruction::ICmp:
      return true;
    }
  };

  for (auto &BB : *F)
    for (auto &II : BB)
      if (IsFixupCandidate(II.getOpcode()))
        WorkList.push_back(&II);

  for (auto Inst : WorkList) {
    switch (Inst->getOpcode()) {
    default:
      llvm_unreachable("Unhandled opcode in IR Fixups");
    case Instruction::ICmp:
      FixupICmp(Inst);
      break;
    }
  }
}

bool JVMIRDecorator::ProcessFunction() {
  LLVM_DEBUG(dbgs() << "*** Begin JVM IR Decoration Pass ***\n");
  // HandleCompositeTypes(Func);

  ConstantFoldInstructions(Func);
  LLVM_DEBUG(dbgs() << "*** IR Dump After Constant folding instructions ***\n");
  Func->dump();

  HandleGlobalConstants(Func);
  LLVM_DEBUG(dbgs() << "*** IR Dump After JVM Global Constants Handler  ***\n");
  Func->dump();

  HandleBitCastOperation(Func);
  LLVM_DEBUG(dbgs() << "*** IR Dump After JVM Casting Handlers  ***\n");
  Func->dump();

  FixupCompositeIndexes(Func);
  LLVM_DEBUG(dbgs() << "*** IR Dump After JVM Fixup Composite Indexes ***\n");
  Func->dump();

  LinearizeGEPChains(Func);
  LLVM_DEBUG(dbgs() << "*** IR Dump After JVM Linearize GEP Chains ***\n");
  Func->dump();

  UniquifyGEPOperands(Func);
  LLVM_DEBUG(dbgs() << "*** IR Dump After JVM GEP Uniquification  ***\n");
  Func->dump();

  PerformIRFixups(Func);
  LLVM_DEBUG(dbgs() << "*** IR Dump JVM IR Fixups  ***\n");
  Func->dump();

  LowerSELECT(Func);
  LLVM_DEBUG(dbgs() << "*** IR Dump After JVM Select Lowering  ***\n");
  Func->dump();

  FuncDecoratedCounter++;

  return true;
}
} // namespace

char JVMIRDecorator::ID = 0;
static RegisterPass<JVMIRDecorator> IRDecodator("jvm-ir-decorator",
                                                "JVM IR Decoration Pass");

namespace llvm {
FunctionPass *createJVMIRDecorator() { return new JVMIRDecorator(); }
} // namespace llvm
