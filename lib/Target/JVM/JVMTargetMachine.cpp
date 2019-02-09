//===- JVMTargetMachine.cpp - Define TargetMachine for JVM -==//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
///
/// \file
/// \brief This file defines the JVM-specific subclass of TargetMachine.
///
//===----------------------------------------------------------------------===//

#include "JVMTargetMachine.h"
#include "JVM.h"
#include "JVMTargetObjectFile.h"
#include "MCTargetDesc/JVMMCTargetDesc.h"
#include "llvm/CodeGen/MachineFunctionPass.h"
#include "llvm/CodeGen/Passes.h"
#include "llvm/CodeGen/RegAllocRegistry.h"
#include "llvm/CodeGen/TargetPassConfig.h"
#include "llvm/IR/Function.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/TargetRegistry.h"
#include "llvm/Target/TargetOptions.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/Transforms/InstCombine/InstCombine.h"

using namespace llvm;

#define DEBUG_TYPE "jvm"

// JVM Optimization
static cl::opt<bool> JvmAggOpt("jvm-aggressive-opts",
                               cl::desc("JVM Aggressive Optimization."),
                               cl::init(false));

static cl::opt<bool> JvmClient("jvm-client",
                               cl::desc("IR to Java Translation."),
                               cl::init(false));

static cl::opt<bool> JvmLSOpt("jvm-ls-opt",
                               cl::desc("JVM load store optimization."),
                               cl::init(false));

extern "C" void LLVMInitializeJVMTarget() {
  // Register the target.
  RegisterTargetMachine<JVMTargetMachine> X(getTheJVMTarget32());
  RegisterTargetMachine<JVMTargetMachine> Y(getTheJVMTarget64());
}

//===----------------------------------------------------------------------===//
// JVM Lowering public interface.
//===----------------------------------------------------------------------===//

static Reloc::Model getEffectiveRelocModel(Optional<Reloc::Model> RM) {
  if (!RM.hasValue())
    return Reloc::PIC_;
  return *RM;
}

void JVMTargetMachine::doCommandLineValidation() {
  bool dumpclassfile;
  cl::GetCommandLineOption<bool>("dump-class-file", dumpclassfile);
  bool enablejvmassembler;
  cl::GetCommandLineOption<bool>("enable-jvm-assembler", enablejvmassembler);

  if (dumpclassfile && !enablejvmassembler)
    llvm::errs() << "Warning : -dump-class-file works only in conjunction "
                    "with -enable-jvm-assembler.\n";
}

/// Create an JVM architecture model.
///
JVMTargetMachine::JVMTargetMachine(const Target &T, const Triple &TT,
                                   StringRef CPU, StringRef FS,
                                   const TargetOptions &Options,
                                   Optional<Reloc::Model> RM,
                                   Optional<CodeModel::Model> CM,
                                   CodeGenOpt::Level OL, bool JIT)
    : LLVMTargetMachine(T,
                        TT.isArch64Bit() ? "E-p:32:32-i:32:32-f:32:32"
                                         : "E-p:32:32-i:32:32-f:32:32",
                        TT, CPU, FS, Options, getEffectiveRelocModel(RM),
                        CM ? *CM : CodeModel::Large, OL),
      TLOF(static_cast<TargetLoweringObjectFile *>(new JVMTargetObjectFile())) {
  this->Options.TrapUnreachable = true;

  // JVM treats each function as an independent unit. Force
  // -ffunction-sections, effectively, so that we can emit them independently.
  this->Options.FunctionSections = true;
  this->Options.DataSections = true;
  this->Options.UniqueSectionNames = true;
  this->Options.MCOptions.AsmVerbose = false;

  // Disabling FastISel for JVM.
  setFastISel(false);
  setO0WantsFastISel(false);

  initAsmInfo();

  // Note that we don't use setRequiresStructuredCFG(true). It disables
  // optimizations than we're ok with, and want, such as critical edge
  // splitting and tail merging.
}

JVMTargetMachine::~JVMTargetMachine() {}

const JVMSubtarget *
JVMTargetMachine::getSubtargetImpl(const Function &F) const {
  Attribute CPUAttr = F.getFnAttribute("target-cpu");
  Attribute FSAttr = F.getFnAttribute("target-features");

  std::string CPU = !CPUAttr.hasAttribute(Attribute::None)
                        ? CPUAttr.getValueAsString().str()
                        : TargetCPU;
  std::string FS = !FSAttr.hasAttribute(Attribute::None)
                       ? FSAttr.getValueAsString().str()
                       : TargetFS;

  auto &I = SubtargetMap[CPU + FS];
  if (!I) {
    // This needs to be done before we create a new subtarget since any
    // creation will depend on the TM and the code generation flags on the
    // function that reside in TargetOptions.
    resetTargetOptions(F);
    I = llvm::make_unique<JVMSubtarget>(TargetTriple, CPU, FS, *this);
  }
  return I.get();
}

namespace {
/// JVM Code Generator Pass Configuration Options.
class JVMPassConfig final : public TargetPassConfig {
public:
  JVMPassConfig(JVMTargetMachine &TM, PassManagerBase &PM)
      : TargetPassConfig(TM, PM) {}

  JVMTargetMachine &getJVMTargetMachine() const {
    return getTM<JVMTargetMachine>();
  }

  FunctionPass *createTargetRegisterAllocator(bool) override;

  void addIRPasses() override;
  void addCodeGenPrepare() override;
  bool addInstSelector() override;
  void addPostRegAlloc() override;
  bool addGCPasses() override { return false; }
  void addPreEmitPass() override;
  void addOptimizedRegAlloc(FunctionPass *RegAllocPass) override;
  void addMachinePasses() override;
};
} // end anonymous namespace

TargetPassConfig *JVMTargetMachine::createPassConfig(PassManagerBase &PM) {
  return new JVMPassConfig(*this, PM);
}

FunctionPass *JVMPassConfig::createTargetRegisterAllocator(bool) {
  return nullptr; // No reg alloc
}

//===----------------------------------------------------------------------===//
// The following functions are called from lib/CodeGen/Passes.cpp to modify
// the CodeGen pass sequence.
//===----------------------------------------------------------------------===//

void JVMPassConfig::addIRPasses() {
  TargetPassConfig::addIRPasses();
  addPass(createJVMLowerMemoryIntrinsics());
  addPass(createAggressiveDCEPass());
  if (JvmAggOpt)
    addPass(createInstructionCombiningPass());
  addPass(createJVMIRDecorator());
  addPass(createAggressiveDCEPass());
  if (JvmAggOpt)
    addPass(createInstructionCombiningPass());
  if (JvmClient)
    addPass(createX2JavaPass());
}

void JVMPassConfig::addCodeGenPrepare() {
  // Disable all code gene prepare optimizations for now.
}

void JVMPassConfig::addMachinePasses() {
  //TODO: Add machine passes incrementally as needed.
  addPass(&PHIEliminationID, false);
  addPass(createJVMPreAllocationFixups());
  addPass(createJVMCopyElision());
  addPreEmitPass();
}

bool JVMPassConfig::addInstSelector() {
  (void)TargetPassConfig::addInstSelector();
  addPass(createJVMISelDag(getJVMTargetMachine(), getOptLevel()));
  return false;
}

void JVMPassConfig::addOptimizedRegAlloc(FunctionPass *RegAllocPass) {
  disablePass(&RegisterCoalescerID);
  TargetPassConfig::addOptimizedRegAlloc(RegAllocPass);
}

void JVMPassConfig::addPostRegAlloc() {
  // TODO: The following CodeGen passes don't currently support code containing
  // virtual registers. Consider removing their restrictions and re-enabling
  // them.

  // Has no asserts of its own, but was not written to handle virtual regs.
  disablePass(&ShrinkWrapID);

  // These functions all require the NoVRegs property.
  disablePass(&MachineCopyPropagationID);
  disablePass(&PostRASchedulerID);
  disablePass(&FuncletLayoutID);
  disablePass(&StackMapLivenessID);
  disablePass(&LiveDebugValuesID);
  disablePass(&PatchableFunctionID);
  disablePass(&PrologEpilogCodeInserterID);

  TargetPassConfig::addPostRegAlloc();
}

void JVMPassConfig::addPreEmitPass() {
  TargetPassConfig::addPreEmitPass();
  if (JvmLSOpt)
    addPass(createJVMLoadStoreEliminationOpt());
  addPass(createJVMOffsetAllocator());
  addPass(createJVMPostAllocationFixups());
}
