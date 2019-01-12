//===------------------ JVMTranslationPass.cpp
//-------------------------------------------------------------------------===//
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

#include "llvm-c/Core.h"
#include "llvm-c/TargetMachine.h"
#include "llvm/ADT/APInt.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/DenseSet.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Analysis/AssumptionCache.h"
#include "llvm/Analysis/BranchProbabilityInfo.h"
#include "llvm/Analysis/InlineCost.h"
#include "llvm/Analysis/LoopInfo.h"
#include "llvm/Analysis/MemorySSA.h"
#include "llvm/Analysis/ScalarEvolution.h"
#include "llvm/Analysis/ScalarEvolutionExpressions.h"
#include "llvm/Analysis/TargetLibraryInfo.h"
#include "llvm/Analysis/Utils/Local.h"
#include "llvm/CodeGen/JVMCandidateChecker.h"
#include "llvm/CodeGen/JVMCodeExtractor.h"
#include "llvm/CodeGen/JVMCommonUtils.h"
#include "llvm/CodeGen/TargetLowering.h"
#include "llvm/CodeGen/TargetPassConfig.h"
#include "llvm/CodeGen/TargetSubtargetInfo.h"
#include "llvm/IR/Argument.h"
#include "llvm/IR/Attributes.h"
#include "llvm/IR/CallSite.h"
#include "llvm/IR/ConstantRange.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DIBuilder.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Dominators.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/InstIterator.h"
#include "llvm/IR/Instruction.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/IntrinsicInst.h"
#include "llvm/IR/Intrinsics.h"
#include "llvm/IR/MDBuilder.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Use.h"
#include "llvm/IR/User.h"
#include "llvm/IR/Value.h"
#include "llvm/Pass.h"
#include "llvm/PassSupport.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/MathExtras.h"
#include "llvm/Support/Threading.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Transforms/Utils/BasicBlockUtils.h"
#include "llvm/Transforms/Utils/Cloning.h"
#include "llvm/Transforms/Utils/ValueMapper.h"
#include <algorithm>
#include <cassert>
#include <cstdint>
#include <exception>
#include <string>
#include <utility>

using namespace llvm;

#define DEBUG_TYPE "jvm-translation"
#define IOBUFSIZE 256

namespace llvm {

STATISTIC(NumXFunctions, "Total number of translated functions");
} // namespace llvm

extern "C" {

enum WrapperKind { WK_STATIC_METHOD = 0, WK_INST_METHOD, WK_INST_METHOD_VAR };

struct ThreadData {
  char *ErrorMsg;
  LLVMModuleRef ModRef;
  LLVMMemoryBufferRef OutMemBuf;

  ThreadData() : ErrorMsg(nullptr) {}
};

void InvokeJVMBackend(void *Packet) {
  LLVMMemoryBufferRef OutMemBuf;
  llvm::set_thread_name("JVM Backend");
  ThreadData *TD = static_cast<ThreadData *>(Packet);

  char *ErrorMsg = TD->ErrorMsg;
  LLVMModuleRef &ModRef = TD->ModRef;
  LLVMTargetRef Target = LLVMGetTargetFromName("jvm32");

  LLVMTargetMachineRef TargetMachine = LLVMCreateTargetMachine(
      Target, "jvm32-unknown-unknown", nullptr, nullptr,
      LLVMCodeGenLevelDefault, LLVMRelocDefault, LLVMCodeModelDefault);

  LLVMTargetMachineEmitToMemoryBuffer(TargetMachine, ModRef, LLVMAssemblyFile,
                                      &ErrorMsg, &OutMemBuf);
  TD->OutMemBuf = std::move(OutMemBuf);
}
}

namespace {

class JVMTranslationPass : public FunctionPass {
public:
  using GlobalConstants = DenseMap<GlobalVariable *, GlobalVariable *>;
  using StringConstants = DenseMap<StringRef, GlobalVariable *>;

  JVMTranslationPass() : FunctionPass(ID) {}

  void getAnalysisUsage(AnalysisUsage &AU) const override {
    AU.addRequired<TargetPassConfig>();
    AU.addRequired<MemorySSAWrapperPass>();
  }

  bool runOnFunction(Function &F) override {
    WK = WK_STATIC_METHOD;
    TM = &getAnalysis<TargetPassConfig>().getTM<TargetMachine>();
    MSSA = &getAnalysis<MemorySSAWrapperPass>().getMSSA();

    bool IsJVMTarget = ((TM->getTargetTriple().getArch() == Triple::jvm32) ||
                        (TM->getTargetTriple().getArch() == Triple::jvm64));

    GCMap.clear();
    Checker.setFunction(F);
    if (!Checker.IsCandidateForTranslation(*MSSA)) {
      DEBUG(dbgs() << "[JVMTranslationPass] Function: \"" << F.getName()
                   << "\" not a candidate for JVM translation.\n");
      if (IsJVMTarget)
        exit(-1);

      return false;
    }

    if (IsJVMTarget)
      return false;

    DEBUG(dbgs() << "[JVMTranslationPass] Function: " << F.getName() << "\n");
    return ProcessForTranslation(F);
  }

  bool ProcessForTranslation(Function &F);

  static char ID; // Pass identification, replacement for typeid..
private:
  Function *GenerateArgumentInitCode(Function *F);
  std::string EmitVMCode(Module *OldMod, const Function *F);
  Function *PruneFunctionBody(Function *F, std::string &ClassFileName,
                              std::string &MethodName);
  void CollectFunctionDependencies(Module *Old, Module *New, const Function *F);
  void CollectGlobalUsers(Value *user, const Function *F,
                          DenseSet<Value *> &Worklist);
  void CopyReturnValueToArgBuffer(Function *F, Value *NewArgBuf);
  bool IsValidDependency(Value *Dep);

  const char *GenWrapperName();

private:
  const TargetMachine *TM = nullptr;
  CandidateChecker Checker;
  CodeLetExtractor Extractor;
  GlobalConstants GCMap;
  StringConstants StrConstMap;
  WrapperKind WK;
  MemorySSA *MSSA;
};

const char *JVMTranslationPass::GenWrapperName() {
  switch (WK) {
  default:
    return "jvm_wrapper_static_method_call";
  case WK_INST_METHOD:
    return "jvm_wrapper_inst_method_call";
  case WK_INST_METHOD_VAR:
    return "jvm_wrapper_inst_method_var_call";
  }
}

Function *JVMTranslationPass::PruneFunctionBody(Function *F,
                                                std::string &classFileName,
                                                std::string &methodName) {
  // Cleanup the function body.
  LLVMContext &Cxt = F->getContext();
  Type *Int8Ty = Type::getInt8Ty(Cxt);
  Type *Int8PtrTy = PointerType::get(Int8Ty, 0);
  const DataLayout &DL = F->getParent()->getDataLayout();
  F->deleteBody();
  BasicBlock *Entry = BasicBlock::Create(F->getContext(), "entry", F);
  IRBuilder<> IRB(Entry);

  // Allocate a new argument byte array and copy the parameters being
  // passed to the function to it.
  Type *ArgByteArrTy = ArrayType::get(Int8Ty, IOBUFSIZE);
  Type *ArgByteArrPtrTy = PointerType::get(ArgByteArrTy, 0);
  Value *ArgByteArr = IRB.CreateAlloca(ArgByteArrTy, nullptr, "ByteArrArg");

  unsigned Offset = 0;
  Type *IndexTy = GetTargetIndexType(DL, Cxt);
  Value *Zero = ConstantInt::get(IndexTy, 0);
  for (auto &Arg : F->args()) {
    Type *ArgTy = Arg.getType();
    uint64_t ArgSz = DL.getTypeSizeInBits(ArgTy) / 8;
    Value *ArgByteArrAtOffset =
        IRB.CreateGEP(ArgByteArr, {Zero, ConstantInt::get(IndexTy, Offset)});
    Value *ArgAlloca =
        IRB.CreateAlloca(Arg.getType(), nullptr, Arg.getName() + ".addr");
    IRB.CreateStore(&Arg, ArgAlloca);
    Value *BitCastedArg = IRB.CreateBitCast(ArgAlloca, Int8PtrTy);
    IRB.CreateMemCpy(ArgByteArrAtOffset, BitCastedArg, ArgSz, 4);
    Offset += ArgSz;
  }

  Twine Name(GenWrapperName());
  Value *StrConstant = nullptr;
  if (StrConstMap.find(classFileName) == StrConstMap.end()) {
    Constant *ClassName =
        ConstantDataArray::getString(F->getContext(), classFileName);
    Type *Ty = cast<ConstantDataArray>(ClassName)->getType();
    StrConstMap[classFileName] = new GlobalVariable(
        *F->getParent(), Ty, true, GlobalValue::PrivateLinkage, ClassName);
  }
  StrConstant = StrConstMap[classFileName];

  Value *ClassName = IRB.CreateAlloca(Int8PtrTy, nullptr, "ClassName");
  Value *StrConstElm0 = IRB.CreateGEP(StrConstant, {Zero, Zero});
  IRB.CreateStore(StrConstElm0, ClassName);
  Value *ClassNameArg = IRB.CreateLoad(ClassName);

  if (StrConstMap.find(methodName) == StrConstMap.end()) {
    Constant *MethodName =
        ConstantDataArray::getString(F->getContext(), methodName);
    Type *Ty = cast<ConstantDataArray>(MethodName)->getType();
    StrConstMap[methodName] = new GlobalVariable(
        *F->getParent(), Ty, true, GlobalValue::PrivateLinkage, MethodName);
  }
  StrConstant = StrConstMap[methodName];

  Value *MethodName = IRB.CreateAlloca(Int8PtrTy, nullptr, "MethodName");
  StrConstElm0 = IRB.CreateGEP(StrConstant, {Zero, Zero});
  IRB.CreateStore(StrConstElm0, MethodName);
  Value *MethodNameArg = IRB.CreateLoad(MethodName);

  FunctionType *WrapperFT = FunctionType::get(
      IRB.getVoidTy(),
      {ArgByteArrPtrTy, ClassNameArg->getType(), MethodNameArg->getType()},
      false);
  Value *WrapperFunc = Function::Create(
      WrapperFT, llvm::GlobalValue::ExternalLinkage, Name, F->getParent());
  IRB.CreateCall(WrapperFunc, {ArgByteArr, ClassNameArg, MethodNameArg});

  Value *RetArg = IRB.CreateAlloca(F->getReturnType());
  Value *CastedRetArg = IRB.CreateBitCast(RetArg, IRB.getInt8PtrTy());
  IRB.CreateMemCpy(CastedRetArg, ArgByteArr,
                   DL.getTypeSizeInBits(F->getReturnType()) / 8, 4);
  Value *RetVal = IRB.CreateLoad(RetArg);
  IRB.CreateRet(RetVal);
  return F;
}

bool JVMTranslationPass::IsValidDependency(Value *Dep) {
  if (isa<GlobalVariable>(Dep) && cast<GlobalVariable>(Dep)->isConstant())
    return true;

  return false;
}

void JVMTranslationPass::CollectGlobalUsers(Value *user, const Function *F,
                                            DenseSet<Value *> &Worklist) {
  if (isa<Instruction>(user) &&
      cast<Instruction>(user)->getParent()->getParent() == F) {
    if (Worklist.find(user) == Worklist.end())
      Worklist.insert(user);
  } else if (isa<Operator>(user)) {
    for (auto Opuser : cast<Operator>(user)->users())
      CollectGlobalUsers(Opuser, F, Worklist);
  }
}

void JVMTranslationPass::CollectFunctionDependencies(Module *Old, Module *New,
                                                     const Function *F) {
  for (auto &Glob : Old->globals()) {
    if (IsValidDependency(&Glob)) {
      GlobalVariable *NewGV = nullptr;
      DenseSet<Value *> Worklist;
      for (auto user : Glob.users())
        CollectGlobalUsers(user, F, Worklist);

      WK = Worklist.size() ? WK_INST_METHOD : WK;

      for (auto u : Worklist) {
        SmallVector<std::pair<unsigned, MDNode *>, 1> MDs;
        NewGV = GCMap.find(&Glob) != GCMap.end() ? GCMap[&Glob] : nullptr;
        if (!NewGV) {
          NewGV = new GlobalVariable(
              *New, Glob.getValueType(), Glob.isConstant(), Glob.getLinkage(),
              (Constant *)nullptr, Glob.getName(), (GlobalVariable *)nullptr,
              Glob.getThreadLocalMode(), Glob.getType()->getAddressSpace());
          GCMap[&Glob] = NewGV;

          if (Glob.hasInitializer()) {
            ValueToValueMapTy VMap;
            NewGV->setInitializer(MapValue(Glob.getInitializer(), VMap));
          }

          Glob.getAllMetadata(MDs);
          for (auto MD : MDs) {
            ValueToValueMapTy VMap;
            NewGV->addMetadata(
                MD.first, *MapMetadata(MD.second, VMap, RF_MoveDistinctMDs));
          }
          NewGV->copyAttributesFrom(&Glob);
        }
        User *user = cast<User>(u);
        for (unsigned i = 0; i < user->getNumOperands(); i++)
          if (user->getOperand(i) == &Glob)
            user->setOperand(i, NewGV);
          else if (isa<Operator>(user->getOperand(i))) {
            Operator *Operand = cast<Operator>(user->getOperand(i));
            for (unsigned j = 0; j < Operand->getNumOperands(); j++)
              if (Operand->getOperand(j) == &Glob)
                Operand->setOperandUnwrapped(j, NewGV);
          }
      }
    }
  }
}

std::string JVMTranslationPass::EmitVMCode(Module *OldMod, const Function *F) {
  ThreadData TD;

  LLVMContextRef TCxt = LLVMContextCreate();
  LLVMModuleRef NewModRef =
      LLVMModuleCreateWithNameInContext("AoCTopLevelClass", TCxt);

  Module *Mod = unwrap(NewModRef);
  CollectFunctionDependencies(OldMod, Mod, F);

  const_cast<Function *>(F)->removeFromParent();
  bool FuncAdded = Mod->checkAndInsertFunction(F);
  assert(FuncAdded && "Cannot add function to module");
  TD.ModRef = std::move(NewModRef);

  DEBUG(
      dbgs() << "\n*** IR Dump After Translation Candidate Modification ***\n");
  Mod->dump();

  DEBUG(dbgs() << "[JVMTranslationPass] Launching VM translation for function: "
               << F->getName() << "\n");

  llvm::llvm_execute_on_thread(InvokeJVMBackend, static_cast<void *>(&TD), 0);

  StringRef ByteCode = unwrap(TD.OutMemBuf)->getBuffer();
  DEBUG(dbgs() << "[JVMTranslationPass] Byte code buffer for function: "
               << "\n"
               << ByteCode << "\n");

  std::error_code EC;
  raw_fd_ostream BCFile("bytecode.s", EC, sys::fs::F_Text);
  BCFile << ByteCode;
  BCFile.close();

  LLVMContextDispose(TCxt);
  return "AoCTopLevelClass";
}

void JVMTranslationPass::CopyReturnValueToArgBuffer(Function *F,
                                                    Value *NewArgBuf) {
  LLVMContext &Cxt = F->getContext();
  Type *Int8Ty = Type::getInt8Ty(Cxt);
  Type *Int8PtrTy = PointerType::get(Int8Ty, 0);
  const DataLayout &DL = F->getParent()->getDataLayout();

  SmallVector<Value *, 4> RetStmts;
  for (auto &BB : *F)
    for (auto &II : BB)
      if (isa<ReturnInst>(&II))
        RetStmts.push_back(&II);

  for (auto Ret : RetStmts) {
    Instruction * R = cast<ReturnInst>(Ret);
    IRBuilder<> IRB(cast<Instruction>(R));
    assert(R->getNumOperands() == 1 && "Void Return encountered");

    Value *RetVal = R->getOperand(0);
    Value *RetValMem = IRB.CreateAlloca(RetVal->getType());
    IRB.CreateStore(RetVal, RetValMem);
    Value *CastedRetMem = IRB.CreateBitCast(RetValMem, Int8PtrTy);
    IRB.CreateMemCpy(NewArgBuf, CastedRetMem,
                     DL.getTypeSizeInBits(RetVal->getType()) / 8, 4);
    IRB.CreateRetVoid();
    R->dropAllReferences();
    R->eraseFromParent();
  }
}

Function *JVMTranslationPass::GenerateArgumentInitCode(Function *F) {
  LLVMContext &Cxt = F->getContext();
  Type *Int8Ty = Type::getInt8Ty(Cxt);
  Type *Int8PtrTy = PointerType::get(Int8Ty, 0);
  IRBuilder<> IRB(&*F->getEntryBlock().begin());
  const DataLayout &DL = F->getParent()->getDataLayout();

  Argument *NewArgBuf =
      new Argument(PointerType::get(ArrayType::get(Int8Ty, IOBUFSIZE), 0),
                   "ByteArrParam", F, 0);
  NewArgBuf->addAttr(Attribute::JVMArgBuf);

  unsigned Offset = 0;
  Type *IndexTy = GetTargetIndexType(DL, Cxt);
  Value *Zero = ConstantInt::get(IndexTy, 0);
  Value *InitVal = ConstantInt::get(Int8Ty, 0);

  // Allocate a local variable for each argument.
  for (auto &Arg : F->args()) {
    Type *ArgTy = Arg.getType();
    std::string Name = Arg.getName();
    Name += "_local";

    Value *Local = IRB.CreateAlloca(ArgTy, nullptr, Name);
    Type *LocalTy = cast<PointerType>(Local->getType())->getElementType();

    uint64_t LocalSz = DL.getTypeSizeInBits(LocalTy) / 8;
    Value *ArgBufAtOffset =
        IRB.CreateGEP(NewArgBuf, {Zero, ConstantInt::get(IndexTy, Offset)});
    Value *CastedLocal = IRB.CreateBitCast(Local, Int8PtrTy);

    // Initialize local variable.
    IRB.CreateMemSet(CastedLocal, InitVal, LocalSz, 4);

    // Copy memory from argument buffer to newly created local variable.
    IRB.CreateMemCpy(CastedLocal, ArgBufAtOffset, LocalSz, 4);
    Offset += LocalSz;

    Value *LoadLocal = IRB.CreateLoad(LocalTy, Local);
    Arg.replaceAllUsesWith(LoadLocal);
  }

  // If function returns a non-void value copy it into original argument
  // buffer.
  if (!F->getReturnType()->isVoidTy())
    CopyReturnValueToArgBuffer(F, NewArgBuf);

  F->clearArguments();
  FunctionType *NewFT =
      FunctionType::get(Type::getVoidTy(Cxt), {NewArgBuf->getType()}, false);
  F->setValueType(NewFT);
  F->setArguments(NewArgBuf, 1);
  return F;
}

bool JVMTranslationPass::ProcessForTranslation(Function &F) {
  ValueToValueMapTy VMap;
  Function *Clone = CloneFunction(&F, VMap);
  std::string MethodName("jvm_");
  MethodName = MethodName + F.getName().str();
  Clone->setName(MethodName.c_str());

  // Clone will be passed only one parameter which is argument
  // byte array.
  Clone = GenerateArgumentInitCode(Clone);
  // try {
  std::string ClassFileName =
      EmitVMCode(const_cast<Module *>(F.getParent()), Clone);
  PruneFunctionBody(&F, ClassFileName, MethodName);
  NumXFunctions++;
  //} catch (std::exception e) {
  //  llvm::errs() << "Caught exception while VM code generation" << e.what()
  //               << "\n";
  //}
  return true;
}
} // end anonymous namespace

char JVMTranslationPass::ID = 0;

namespace llvm {
FunctionPass *createJVMTranslationPass() { return new JVMTranslationPass(); }
} // namespace llvm

llvm::RegisterPass<JVMTranslationPass> TranslationPass("jvm-translation-pass",
                                                       "JVM Translation Pass");
