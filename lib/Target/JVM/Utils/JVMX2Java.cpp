//===------------- XToJavaPass.cpp - IR to Java translator----------------===//
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

#include "llvm/Pass.h"
#include "llvm/Support/raw_ostream.h"

using namespace llvm;

#define DEBUG_TYPE "XtoJava"

STATISTIC(XToJavaCounter, "Counts number of functions greeted");

namespace llvm {

class PHIElimination {
public:
  PHIElimination() {}
  PHIElimination(Function &Func) : F(&Func) {}
  void setFunction(Function &Func) { F = &Func; }
  int ProcessFunction();

private:
  int ProcessBlock(BasicBlock &B);
  std::string genTempName(int ctr, const char *);

  Function *F;
};

std::string PHIElimination::genTempName(int ctr, const char *prefix) {
  char ctrStr[32] = {'\0'};
  std::string Name(prefix);
  sprintf(ctrStr, "%d", ctr);
  Name += ctrStr;
  return Name;
}

int PHIElimination::ProcessBlock(BasicBlock &B) {
  static int newInstrCtr = -1;
  int PhiCnt = 0;
  BasicBlock &entry = F->getEntryBlock();
  for (auto &Inst : B) {
    if (isa<PHINode>(Inst)) {
      PHINode &I = cast<PHINode>(Inst);
      IRBuilder<> IRB(&entry, entry.begin());
      // Create a new temporary into entry block for each
      // PHI node and later assign incoming phi values to
      // it in the incoming blocks.
      AllocaInst *NewVar = IRB.CreateAlloca(
          I.getType(), nullptr, genTempName(++newInstrCtr, "phiAlloc"));
      LoadInst *NewLI = IRB.CreateLoad(I.getType(), NewVar,
                                       genTempName(++newInstrCtr, "phiLocal"));
      I.replaceAllUsesWith(NewLI);

      for (unsigned i = 0; i < I.getNumIncomingValues(); i++) {
        Value *InVal = I.getIncomingValue(i);
        BasicBlock *InBlk = I.getIncomingBlock(i);
        IRB.SetInsertPoint(InBlk->getTerminator());
        // Create a new store before the terminator in each
        // incoming basic block which assigns the incoming
        // value to new temporary created for phi.
        IRB.CreateStore(InVal, NewVar);
      }
      I.removeFromParent();
      PhiCnt++;
    } else
      break;
  }
  return PhiCnt;
}

int PHIElimination::ProcessFunction() {
  int PhiCnt = 0;
  ReversePostOrderTraversal<Function *> RPOT(F);
  for (BasicBlock *BB : RPOT)
    PhiCnt += ProcessBlock(*BB);
  return PhiCnt;
}

#define GRAY 1
#define BLACK 2

class BlockRevPostOrderTraversal {
public:
  using BlockMap = DenseMap<BasicBlock *, unsigned>;
  using BlockArray = SmallVector<BasicBlock *, 32>;
  using iterator = SmallVector<BasicBlock *, 32>::reverse_iterator;

  BlockRevPostOrderTraversal(Function *F);

  iterator begin() { return Blocks.rbegin(); }
  iterator end() { return Blocks.rend(); }

private:
  void visitBlocks(BasicBlock *);

  BlockMap OrderedMap;
  BlockArray Blocks;
};

void BlockRevPostOrderTraversal::visitBlocks(BasicBlock *Blk) {
  // To avoid visiting crossedges and backedges.
  if (OrderedMap.find(Blk) != OrderedMap.end() &&
      (OrderedMap[Blk] == GRAY || OrderedMap[Blk] == BLACK))
    return;

  OrderedMap[Blk] = GRAY;

  Instruction *TI = Blk->getTerminator();
  if (isa<BranchInst>(TI) && cast<BranchInst>(TI)->isConditional()) {
    BasicBlock *FalseBlock = cast<BasicBlock>(TI->getSuccessor(1));
    visitBlocks(FalseBlock);

    BasicBlock *TrueBlock = cast<BasicBlock>(TI->getSuccessor(0));
    visitBlocks(TrueBlock);
  } else {
    for (unsigned i = 0; i < TI->getNumSuccessors(); i++) {
      visitBlocks(TI->getSuccessor(i));
    }
  }
  OrderedMap[Blk] = BLACK;
  Blocks.push_back(Blk);
}

BlockRevPostOrderTraversal::BlockRevPostOrderTraversal(Function *F) {
  visitBlocks(&F->getEntryBlock());
}

/// If block has more than one predecessor then it must be predicated.
/// If block's predecessor is not its RPOT predecessor then it must be
/// predicated.
/// For each conditional branch.
/// a/ Propagate the true and false conditions to the respective target
///    blocks.
/// For each un-conditional branch.
/// a/ Create new condition flag for that branch in the function.
/// b/ Add the condition flag to the predication condition of the target
///    block.
///
class BlockPredication {
public:
  using BlockOrdering = ReversePostOrderTraversal<Function *>;
  using BlockOrderList = std::list<BasicBlock *>;
  using BlockOrderListItr = std::list<BasicBlock *>::iterator;
  using PredicateVar = PointerIntPair<Value *, 1>;
  using PredicateArr = SmallVector<PredicateVar, 2>;
  using PredicateCondition = DenseMap<BasicBlock *, PredicateArr>;

  BlockPredication() {}
  BlockPredication(Function &Func) : F(&Func) {}
  BlockOrderList &getOrderedList() { return blockList; }
  void ProcessFunction();
  void setFunction(Function &Func) { F = &Func; }

private:
  void ProcessBlocks();
  bool IsRPOTPredecessor(BlockOrderListItr Block, BasicBlock *Pred);
  bool CanSkipPredication(BlockOrderListItr Itr);
  void ProcessBranchInst(BranchInst *BI);
  void ProcessSwitchInst(SwitchInst *SI);
  void InsertPredicate(BasicBlock *Blk, Value *Pred, int TrueCondF = 1);
  void VerifyPredication();

  std::string genTempName(std::string Prefix);
  void DumpPredication(BasicBlock *Blk);

  BlockOrderList blockList;
  PredicateCondition PredCondMap;
  Function *F;
};

bool BlockPredication::IsRPOTPredecessor(BlockOrderListItr Block,
                                         BasicBlock *Pred) {
  if (*(--Block) == Pred)
    return true;
  else
    return false;
}

bool BlockPredication::CanSkipPredication(BlockOrderListItr Itr) {
  BasicBlock *Block = *Itr;
  if (Block->hasOneUse() &&
      IsRPOTPredecessor(
          Itr, cast<BasicBlock>(
                   cast<Instruction>(*(Block->user_begin()))->getParent())))
    return true;
  else
    return false;
}

std::string BlockPredication::genTempName(std::string Prefix) {
  static int ctr = 0;
  char ctrStr[32] = {'\0'};
  std::string Name = "brVar_";
  Name += Prefix;
  sprintf(ctrStr, "_%d", ctr++);
  Name += ctrStr;
  return Name;
}

void BlockPredication::ProcessSwitchInst(SwitchInst *SI) {
  /// Add following compare instruction for each switch label
  /// in the block containing switch
  ///  PV = icmp cond == value
  /// Insert PV in the Predicate map of the destination block
  /// of that switch label.
  llvm_unreachable("Switch terminator currently not handled");
}

void BlockPredication::InsertPredicate(BasicBlock *Blk, Value *Pred,
                                       int TrueCondF) {
  if (PredCondMap.find(Blk) == PredCondMap.end())
    PredCondMap[Blk] = {PredicateVar(Pred, TrueCondF)};
  else
    PredCondMap[Blk].push_back(PredicateVar(Pred, TrueCondF));
}

void BlockPredication::ProcessBranchInst(BranchInst *BI) {
  if (BI->isUnconditional()) {
    BasicBlock &entry = F->getEntryBlock();
    IRBuilder<> IRB(&entry.front());

    Type *BoolTy = Type::getInt1Ty(F->getContext());
    Value *Var = IRB.CreateAlloca(BoolTy, nullptr,
                                  genTempName(BI->getParent()->getName()));

    IRB.SetInsertPoint(BI);
    IRB.CreateStore(Constant::getIntegerValue(BoolTy, APInt(1, 1)), Var);

    InsertPredicate(BI->getSuccessor(0), Var);

  } else {
    assert(BI->isConditional() && BI->getNumSuccessors() == 2 &&
           "Conditional branch having other than two successors");
    Value *Cond = BI->getCondition();
    BasicBlock *TrueBlk = BI->getSuccessor(0);
    BasicBlock *FalseBlk = BI->getSuccessor(1);

    InsertPredicate(TrueBlk, Cond);
    InsertPredicate(FalseBlk, Cond, 0);
  }
}

void BlockPredication::ProcessBlocks() {
  for (BlockOrderListItr Itr = blockList.begin(); Itr != blockList.end();
       Itr++) {
    BasicBlock *Block = *Itr;
    if (CanSkipPredication(Itr))
      continue;
    for (auto TI = Block->user_begin(); TI != Block->user_end(); TI++) {
      if (dyn_cast<BranchInst>(*TI)) {
        ProcessBranchInst(cast<BranchInst>(*TI));
      } else if (dyn_cast<SwitchInst>(*TI)) {
        ProcessSwitchInst(cast<SwitchInst>(*TI));
      } else if (dyn_cast<ReturnInst>(*TI)) {
        // Should not be possible in this algorithm.
        continue;
      } else {
        llvm_unreachable("Unhandled TerminatorInst");
      }
    }
  }
}

void BlockPredication::VerifyPredication() {
  // Check each block has a predicate condition apart from entry block.
  for (BlockOrderListItr Itr = blockList.begin(); Itr != blockList.end();
       Itr++) {
    BasicBlock *Block = *Itr;
    if (Block == &F->getEntryBlock() || CanSkipPredication(Itr))
      continue;
    if (PredCondMap.find(Block) == PredCondMap.end()) {
      std::string error = Block->getName();
      error += " block without a predicate condition found";
      report_fatal_error(error, false);
    }
    DumpPredication(Block);
  }
}

void BlockPredication::DumpPredication(BasicBlock *Blk) {
  int i = 0;
  LLVM_DEBUG(dbgs() << "Block Name : " << Blk->getName());
  for (auto PV : PredCondMap[Blk]) {
    LLVM_DEBUG(dbgs() << "  [" << i++ << "] :" << PV.getPointer()->getName()
                 << (PV.getInt() ? " : ON" : " : OFF"));
  }
}

void BlockPredication::ProcessFunction() {
  BlockOrdering RPOT(F);
  for (BasicBlock *BB : RPOT)
    blockList.push_back(BB);
  ProcessBlocks();
  VerifyPredication();
}
} // namespace llvm

namespace {

class XToJavaPass : public FunctionPass {
public:
  using ValueMap = DenseMap<Value *, Value *>;

  static char ID; // Pass identification, replacement for typeid
  XToJavaPass() : FunctionPass(ID) {}

  bool runOnFunction(Function &F) override {
    std::string Code;
    raw_string_ostream OS(Code);
    Func = &F;
    return GenJavaClass(OS);
  }

  bool GenJavaClass(raw_ostream &OS);

  /// JVM-Server = llc -jvm-server + Jasmin Assembler.
  /// JVM-Client = llc -jvm-client + Javac compiler.

  /// In optimization mode (-jvm-server) llc's JVM backend will be
  /// directly generating code in Jasmin assembly which will cover
  /// entire functionality of following routines.

  /// Following routines are used for emitting JAVA code for translation
  /// candidate in fast suboptimal mode (-jvm-client) which will use javac
  /// as the backend.

  void EmitPrologue(raw_ostream &);
  void EmitBody(raw_ostream &);
  void EmitEpilogue(raw_ostream &);
  void EmitBlock(BasicBlock *, raw_ostream &);

  void HandleCompositeTypes(Function *F);

  /// Following routine will be used by both JVM-Client and JVM-Server
  void EmitArgumentInitializationWrappers();

  /// We don't modify the program, so we preserve all analyses.
  void getAnalysisUsage(AnalysisUsage &AU) const override {
    AU.setPreservesAll();
  }

  Function *CloneFunction(Function *F);
  BasicBlock *CloneBlock(BasicBlock *Blk, ValueMap &VMap);
  BasicBlock *PatchBlock(BasicBlock *Blk, ValueMap &VMap);
  Instruction *CloneInstruction(Instruction *I, ValueMap &VMap);

private:
  PHIElimination PhiEliminator;
  BlockPredication Predicator;
  Function *Func;
};

void XToJavaPass::EmitArgumentInitializationWrappers() {
  /// 1/ It will generate a clone of translation candidate which will have
  ///    just one argument which is a byte array of IOBUF.
  /// 2/ Clone will be modified internally such that all the references
  ///    to arguments are replaced by references from within IOBUF.
  /// 3/ It will generate a stub in "C" which will have dongle initialization
  ///    calls along with call to invoke VM to evaluate clone.
  /// 4/ It will replace the original callsite with call to stub.
  LLVM_DEBUG(dbgs() << "Emitting wrappers");
}

void XToJavaPass::EmitPrologue(raw_ostream &OS) {
  BlockRevPostOrderTraversal RPOT(Func);
  // ReversePostOrderTraversal<Function*> RPOT(Func);
  for (BasicBlock *BB : RPOT)
    LLVM_DEBUG(dbgs() << "Block Name : " << BB->getName() << "\n");
}

void XToJavaPass::EmitEpilogue(raw_ostream &OS) {
  LLVM_DEBUG(dbgs() << "End of translation");
}

void XToJavaPass::EmitBlock(BasicBlock *Blk, raw_ostream &OS) {
  LLVM_DEBUG(dbgs() << "Generating code for ");
  Blk->dump();
}

void XToJavaPass::EmitBody(raw_ostream &OS) {
  int PhiRemoved = PhiEliminator.ProcessFunction();
  LLVM_DEBUG(dbgs() << "Eliminated " << PhiRemoved << " phi nodes");

  // Order them as per dependency.
  Predicator.ProcessFunction();
  for (auto *Blk : Predicator.getOrderedList()) {
    EmitBlock(Blk, OS);
  }
}

bool XToJavaPass::GenJavaClass(raw_ostream &OS) {
  EmitPrologue(OS);

  EmitBody(OS);

  EmitEpilogue(OS);
  XToJavaCounter++;
  return true;
}
} // namespace

char XToJavaPass::ID = 0;

namespace llvm {
FunctionPass *createX2JavaPass() { return new XToJavaPass(); }
} // namespace llvm
