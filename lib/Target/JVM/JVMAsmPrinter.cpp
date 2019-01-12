//===---------------- JVMAsmPrinter.cpp - JVM LLVM assembly writer --------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
///
/// \file
/// \brief This file contains a printer that converts from our internal
/// representation of machine-dependent LLVM code to the JVM assembly
/// language.
///
//===----------------------------------------------------------------------===//

#include "JVMAsmPrinter.h"
#include "InstPrinter/JVMInstPrinter.h"
#include "JVM.h"
#include "JVMMCInstLower.h"
#include "JVMMachineFunctionInfo.h"
#include "JVMRegisterInfo.h"
#include "MCTargetDesc/JVMAssembler.h"
#include "MCTargetDesc/JVMMCTargetDesc.h"
#include "MCTargetDesc/JVMTargetStreamer.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/CodeGen/Analysis.h"
#include "llvm/CodeGen/AsmPrinter.h"
#include "llvm/CodeGen/JVMCommonUtils.h"
#include "llvm/CodeGen/MachineConstantPool.h"
#include "llvm/CodeGen/MachineInstr.h"
#include "llvm/CodeGen/MachineModuleInfoImpls.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/GlobalVariable.h"
#include "llvm/IR/Type.h"
#include "llvm/MC/MCAsmInfo.h"
#include "llvm/MC/MCContext.h"
#include "llvm/MC/MCStreamer.h"
#include "llvm/MC/MCSymbol.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/TargetRegistry.h"
#include "llvm/Support/raw_ostream.h"

using namespace llvm;

#define DEBUG_TYPE "asm-printer"

static cl::opt<bool>
    DumpJVMClassFile("dump-class-file", cl::NotHidden,
                     cl::desc("Dump JVM .class file over disk."));

//===----------------------------------------------------------------------===//
// Helpers.
//===----------------------------------------------------------------------===//

JVMTargetStreamer *JVMAsmPrinter::getTargetStreamer() {
  MCTargetStreamer *TS = OutStreamer->getTargetStreamer();
  return static_cast<JVMTargetStreamer *>(TS);
}

//===----------------------------------------------------------------------===//
// JVMAsmPrinter Implementation.
//===----------------------------------------------------------------------===//
std::string JVMAsmPrinter::EmitTypeString(Type *Ty) {
  if (isa<StructType>(Ty))
    return MFI->getFromTypeNameMap(Ty);
  else if (isa<ArrayType>(Ty))
    return EmitTypeString(cast<ArrayType>(Ty)->getElementType());
  else if (isa<PointerType>(Ty))
    return EmitTypeString(cast<PointerType>(Ty)->getElementType());
  else {
    EVT VT = EVT::getEVT(Ty, 0);
    switch (VT.getSimpleVT().SimpleTy) {
    default:
      assert("Unhandle type in EmitTypeString()");
    case MVT::i1:
      return "boolean";
    case MVT::i8:
      return "byte";
    case MVT::i16:
      return "short";
    case MVT::i32:
      return "int";
    case MVT::i64:
      return "long";
    case MVT::f32:
      return "float";
    case MVT::f64:
      return "double";
    }
  }
}

void JVMAsmPrinter::EmitFieldAllocation(std::string &FieldInit, Type *Ty,
                                        std::string ClassName,
                                        unsigned FieldID) {
  if (isa<ArrayType>(Ty)) {
    char IDStr[8] = {'\0'};
    Type *ElmTy = cast<ArrayType>(Ty)->getElementType();
    unsigned NumElm = cast<ArrayType>(Ty)->getNumElements();
    std::string ElmTyStr = EmitTypeString(ElmTy);
    FieldInit += "\n  aload_0";
    FieldInit += "\n  bipush ";
    sprintf(IDStr, "%d", NumElm);
    FieldInit += IDStr;
    if (isa<StructType>(ElmTy))
      FieldInit += "\n  anewarray ";
    else
      FieldInit += "\n  newarray ";
    FieldInit += ElmTyStr;
    FieldInit += "\n  putfield ";
    FieldInit += ClassName;
    FieldInit += "/f";
    IDStr[0] = '\0';
    sprintf(IDStr, "%d", FieldID);
    FieldInit += IDStr;
    FieldInit += " ";
    MFI->getTypeDescriptor(Ty, FieldInit);
    if (isa<StructType>(ElmTy)) {
      for (unsigned i = 0; i < NumElm; i++) {
        FieldInit += "\n  aload_0";
        FieldInit += "\n  getfield ";
        FieldInit += ClassName;
        FieldInit += "/f";
        IDStr[0] = '\0';
        sprintf(IDStr, "%d", FieldID);
        FieldInit += IDStr;
        FieldInit += " ";
        MFI->getTypeDescriptor(Ty, FieldInit);
        if (i < 6)
          FieldInit += "\n  iconst_";
        else
          FieldInit += "\n  ldc ";
        IDStr[0] = '\0';
        sprintf(IDStr, "%d", i);
        FieldInit += IDStr;
        FieldInit += "\n  new ";
        FieldInit += ElmTyStr;
        FieldInit += "\n  dup";
        FieldInit += "\n  invokenonvirtual ";
        FieldInit += ElmTyStr;
        FieldInit += ".<init>()V";
        FieldInit += "\n  aastore";
      }
    }
  } else if (isa<StructType>(Ty)) {
    char IDStr[8] = {'\0'};
    std::string TypeStr = EmitTypeString(Ty);
    FieldInit += "\n  aload_0";
    FieldInit += "\n  new ";
    FieldInit += TypeStr;
    FieldInit += "\n  dup";
    FieldInit += "\n  invokenonvirtual ";
    FieldInit += TypeStr;
    FieldInit += ".<init>()V";
    FieldInit += "\n  putfield ";
    FieldInit += ClassName;
    FieldInit += "/f";
    sprintf(IDStr, "%d", FieldID);
    FieldInit += IDStr;
    FieldInit += " ";
    MFI->getTypeDescriptor(Ty, FieldInit);
  }
}

void JVMAsmPrinter::EmitClassFields(std::string &StrBuf, StructType *STy,
                                    FieldTypeIndexVec &CompFields) {
  // Emit fields.
  for (unsigned i = 0; i < STy->getNumElements(); i++) {
    char fieldBuf[8] = {'\0'};
    StrBuf += "\n.field  public  f";
    sprintf(fieldBuf, "%d", i);
    StrBuf += fieldBuf;
    StrBuf += " ";
    Type *FieldTy = STy->getElementType(i);
    MFI->getTypeDescriptor(FieldTy, StrBuf);
    if (isa<CompositeType>(FieldTy))
      CompFields.push_back(std::make_pair(FieldTy, i));
  }
}

std::string JVMAsmPrinter::GetLoadString(Type *Ty) {
  if (isa<StructType>(Ty))
    return "\n  getfield";
  else if (isa<ArrayType>(Ty)) {
    Type *ElmTy = cast<ArrayType>(Ty)->getElementType();
    if (isa<CompositeType>(ElmTy))
      return "\n  aaload";
    return GetLoadString(ElmTy);
  } else {
    const DataLayout &DL = MF->getDataLayout();
    const JVMTargetLowering &TLI =
        *MF->getTarget()
             .getSubtarget<JVMSubtarget>(MF->getFunction())
             .getTargetLowering();
    EVT VT = TLI.getValueType(DL, Ty);
    switch (VT.getSimpleVT().SimpleTy) {
    default:
      llvm_unreachable("Unhandled type in GetLoadString");
    case MVT::i8:
      return "\n  baload";
    case MVT::i16:
      return "\n  saload";
    case MVT::i32:
      return "\n  iaload";
    case MVT::i64:
      return "\n  laload";
    }
  }
}

std::string JVMAsmPrinter::GetStoreString(Type *Ty) {
  if (isa<StructType>(Ty))
    return "\n  putfield";
  else if (isa<ArrayType>(Ty)) {
    Type *ElmTy = cast<ArrayType>(Ty)->getElementType();
    if (isa<CompositeType>(ElmTy))
      return "\n  aastore";
    return GetLoadString(ElmTy);
  } else {
    const DataLayout &DL = MF->getDataLayout();
    const JVMTargetLowering &TLI =
        *MF->getTarget()
             .getSubtarget<JVMSubtarget>(MF->getFunction())
             .getTargetLowering();
    EVT VT = TLI.getValueType(DL, Ty);
    switch (VT.getSimpleVT().SimpleTy) {
    default:
      llvm_unreachable("Unhandled type in GetLoadString");
    case MVT::i8:
      return "\n  bastore";
    case MVT::i16:
      return "\n  sastore";
    case MVT::i32:
      return "\n  iastore";
    case MVT::i64:
      return "\n  lastore";
    }
  }
}

void JVMAsmPrinter::PopulateConstValVector(const Constant *Init,
                                           ConstantVals &CVec) {
  Type *Ty = Init->getType();
  if (isa<ArrayType>(Ty) || isa<StructType>(Ty)) {
    unsigned NumElems = isa<ArrayType>(Ty)
                            ? cast<ArrayType>(Ty)->getNumElements()
                            : cast<StructType>(Ty)->getNumElements();
    for (unsigned i = 0; i < NumElems; i++)
      PopulateConstValVector(Init->getAggregateElement(i), CVec);
  } else
    CVec.push_back(Init);
}

void JVMAsmPrinter::EmitFieldInitialization(std::string &AsmBuf, Type *PTy,
                                            Type *Ty, unsigned &ConstIdx,
                                            ConstantVals &CVec,
                                            unsigned FieldID,
                                            bool IsTopLevelObj) {
  if (IsTopLevelObj)
    AsmBuf += "\n  aload_0";

  if (isa<ArrayType>(Ty)) {
    char IDStr[8] = {'\0'};
    AsmBuf += GetLoadString(PTy);
    if (isa<StructType>(PTy)) {
      AsmBuf += " ";
      AsmBuf += MFI->getFromTypeNameMap(PTy);
      AsmBuf += "/f";
      IDStr[0] = '\0';
      sprintf(IDStr, "%d", FieldID);
      AsmBuf += IDStr;
      AsmBuf += " ";
      MFI->getTypeDescriptor(Ty, AsmBuf);
    }
    Type *ElmTy = cast<ArrayType>(Ty)->getElementType();
    unsigned NumElems = cast<ArrayType>(Ty)->getNumElements();
    for (unsigned i = 0; i < NumElems; i++) {
      if (i < NumElems - 1)
        AsmBuf += "\n  dup";
      AsmBuf += "\n  ldc ";
      IDStr[0] = '\0';
      sprintf(IDStr, "%d", i);
      AsmBuf += IDStr;
      EmitFieldInitialization(AsmBuf, Ty, ElmTy, ConstIdx, CVec, FieldID,
                              false);
    }
  } else if (isa<StructType>(Ty)) {
    char IDStr[8] = {'\0'};
    AsmBuf += GetLoadString(PTy);
    if (isa<StructType>(PTy)) {
      AsmBuf += " ";
      AsmBuf += MFI->getFromTypeNameMap(PTy);
      AsmBuf += "/f";
      IDStr[0] = '\0';
      sprintf(IDStr, "%d", FieldID);
      AsmBuf += IDStr;
      AsmBuf += " ";
      MFI->getTypeDescriptor(Ty, AsmBuf);
    }
    unsigned NumFields = cast<StructType>(Ty)->getNumElements();
    for (unsigned i = 0; i < NumFields; i++) {
      Type *FldTy = cast<StructType>(Ty)->getTypeAtIndex(i);
      if (i < NumFields - 1)
        AsmBuf += "\n  dup";
      EmitFieldInitialization(AsmBuf, FldTy, Ty, ConstIdx, CVec, i, false);
    }
  } else {
    char IDStr[8] = {'\0'};
    assert(isa<ConstantInt>(CVec[ConstIdx]));
    unsigned InitVal = cast<ConstantInt>(CVec[ConstIdx++])->getZExtValue();
    sprintf(IDStr, "%d", InitVal);
    AsmBuf += "\n  ldc ";
    AsmBuf += IDStr;
    AsmBuf += GetStoreString(PTy);
    if (isa<StructType>(PTy)) {
      AsmBuf += MFI->getFromTypeNameMap(PTy);
      AsmBuf += "/f";
      IDStr[0] = '\0';
      sprintf(IDStr, "%d", FieldID);
      AsmBuf += IDStr;
      AsmBuf += " ";
      MFI->getTypeDescriptor(Ty, AsmBuf);
    }
  }
}

std::string JVMAsmPrinter::EmitClassDefinition(Type *Ty,
                                               std::string ClassName) {
  assert(isa<StructType>(Ty) && "Structure type expected");
  StructType *STy = cast<StructType>(Ty);
  FieldTypeIndexVec CompFields;

  std::string ClassDef = "\n";
  ClassDef += ".class public ";
  ClassDef += ClassName;
  ClassDef += "\n.super java/lang/Object";

  EmitClassFields(ClassDef, STy, CompFields);

  // Emit class constructor.
  ClassDef += "\n.method public <init>()V  \
               \n  .limit stack 5  \
               \n  .limit locals 2  \
               \n  aload_0     \
               \n  invokenonvirtual java/lang/Object.<init>()V";
  // Emit field initialization instruction.
  for (auto &TyFIDPair : CompFields)
    EmitFieldAllocation(ClassDef, TyFIDPair.first, ClassName, TyFIDPair.second);
  ClassDef += "\n  return \
               \n.end method\n\n";
  return ClassDef;
}

void JVMAsmPrinter::EmitClassDefinitions() {
  JVMFunctionInfo::TypeNameMap *STyName = MFI->getTypeNameMap();
  for (auto TyNmPair : *STyName) {
    std::string ClassName = *TyNmPair.second;
    std::string ClassDef = EmitClassDefinition(TyNmPair.first, ClassName);
    DEBUG(dbgs() << "Class File : \n" << ClassDef;);

    std::error_code EC;
    sys::fs::OpenFlags OpenFlags = sys::fs::F_None;
    raw_fd_ostream ClassFile(ClassName + ".j", EC, OpenFlags);
    ClassFile << ClassDef;
    ClassFile.close();
  }
}

void JVMAsmPrinter::EmitStartOfAsmFile(Module &M) {
  JVMTargetStreamer *TS = getTargetStreamer();

  std::string ModName(M.getName().data());
  std::string ClassName = ModName.substr(0, ModName.find(".ll"));
  std::string str(".source Dummy.java\n");
  str += ".class public ";
  str += ClassName;
  str += "\n.super java/lang/Object";

  TS->emitString(str);
}

void JVMAsmPrinter::EmitEndOfAsmFile(Module &M) {
  JVMTargetStreamer *TS = getTargetStreamer();
  TS->emitString("; End of AsmFile.");
  DEBUG(llvm::dbgs() << "Finished JASM dumping."
                     << "\n");
  if (TM.getMCAsmInfo()->getEnableAsmInMemoryEmitting()) {
    TS->flush();
    JVMAssembler obj;
    Optional<std::unique_ptr<JVMAssembler::JVMClassPacket>> classPkt =
        obj.AssembleClass(TM.getMCAsmInfo()->getAsmMemoryBuffer());

    if (DumpJVMClassFile && classPkt.hasValue()) {
      std::error_code EC;
      sys::fs::OpenFlags OpenFlags = sys::fs::F_None;
      std::string ModName(M.getName().data());
      std::string ClassName = ModName.substr(0, ModName.find(".ll"));

      raw_fd_ostream ClassFile(ClassName + ".class", EC, OpenFlags);
      ClassFile.write(classPkt->get()->getPayload(),
                      classPkt->get()->getSize());
      ClassFile.close();

    } else if (classPkt.hasValue()) {
      classPkt->get()->dump();
    }
  }
}

void JVMAsmPrinter::EmitFunctionBodyStart() {
  char limit[128];
  SmallVector<MVT, 4> ResultVTs;
  static bool ConstrPrint = false;
  const Function &F = MF->getFunction();
  JVMTargetStreamer *TS = getTargetStreamer();
  const JVMFunctionInfo *MFI = MF->getInfo<JVMFunctionInfo>();

  ComputeLegalVTs(F, TM, F.getReturnType(), ResultVTs);

  assert(ResultVTs.size() <= 1 &&
         "Result value occupying more than one first class type.\n");

  std::string ConstrStr = "";
  if (!ConstrPrint) {
    if (MFI->IsEmitGlobConsts()) {
      ConstantVals CVec;
      unsigned ConstIdx = 0;
      // For Global constant structure object, emit field declaration,
      // allocation and initialization instructions.
      const Module *Mod = F.getParent();
      std::string ModName(Mod->getName().data());
      std::string ClassName = ModName.substr(0, ModName.find(".ll"));

      const GlobalVariable *GCStructObj =
          Mod->getNamedGlobal("ConstantInitStruct");
      Type *GCStructTy = GetRootType(GCStructObj->getType());
      assert(isa<StructType>(GCStructTy));
      StructType *STy = cast<StructType>(GCStructTy);

      FieldTypeIndexVec CompFields;
      EmitClassFields(ConstrStr, STy, CompFields);

      // Emit class constructor.
      ConstrStr += "\n.method public <init>()V \
                    \n  aload_0                \
                    \n  invokenonvirtual java/lang/Object.<init>()V";

      for (auto TyIdx : CompFields)
        EmitFieldAllocation(ConstrStr, TyIdx.first, ClassName, TyIdx.second);

      PopulateConstValVector(GCStructObj->getInitializer(), CVec);
      for (auto TyIdx : CompFields)
        EmitFieldInitialization(ConstrStr, STy, TyIdx.first, ConstIdx, CVec,
                                TyIdx.second, true);

      JVMFunctionInfo::TypeNameMap *STyName =
          const_cast<JVMFunctionInfo *>(MFI)->getTypeNameMap();
      STyName->erase(STy);
    } else {
      ConstrStr += "\n.method public <init>()V \
                    \n  aload_0                \
                    \n  invokenonvirtual java/lang/Object.<init>()V";
    }

    ConstrStr += "\n  return     \
          \n.end method\n";
    ConstrPrint = true;
  }
  TS->emitString(ConstrStr);

  std::string str(".method public static ");
  str += F.getName();
  str += "(";

  for (auto &Arg : F.args()) {
    Type *T = Arg.getType();
    if (isa<PointerType>(T))
      T = cast<PointerType>(T)->getElementType();
    MFI->getTypeDescriptor(T, str);
  }

  Type *RetType = F.getReturnType();

  str += ")";
  MFI->getTypeDescriptor(RetType, str);

  TS->emitString(str);

  // Emit .limit directives.
  str = "\t.limit locals ";
  limit[0] = '\0';
  sprintf(limit, "%d", MFI->getLocalsLimit());
  str += limit;

  TS->emitString(str);

  // TODO: Emit correct stack limits.
  str = "\t.limit stack ";
  str += limit;

  TS->emitString(str);
  AsmPrinter::EmitFunctionBodyStart();
}

void JVMAsmPrinter::EmitFunctionBodyEnd() {
  JVMTargetStreamer *TS = getTargetStreamer();
  TS->emitString(".end method ");

  // TODO: Assemble each class definition into a separate .class file.
  EmitClassDefinitions();
}

// TODO: Do it more elegantly by adding a skipPrint flag in MCInstrDesc.
static bool IsSkipInstruction(const MachineInstr &MI) {
  unsigned Opcode = MI.getOpcode();
  if (MI.getDesc().isRegCConv())
    return true;
  switch (Opcode) {
  default:
    return false;
  case JVM::ARGUMENT_I32:
  case JVM::ARGUMENT_I64:
  case JVM::ARGUMENT_I16:
  case JVM::ARGUMENT_I8:
  case JVM::ARGUMENT_F32:
  case JVM::ARGUMENT_F64:
  case JVM::PRIMALLOC_CSZ:
  case JVM::PRIMALLOC_VSZ:
    return true;
  }
}

void JVMAsmPrinter::EmitInstruction(const MachineInstr *MI) {
  if (IsSkipInstruction(*MI))
    return;

  DEBUG(dbgs() << "EmitInstruction: " << *MI << '\n');

  JVMMCInstLower InstLower(OutContext, *this);
  MCInst TmpInst;
  InstLower.Lower(MI, TmpInst);
  EmitToStreamer(*OutStreamer, TmpInst);
}

const MCExpr *JVMAsmPrinter::lowerConstant(const Constant *CV) {
  return AsmPrinter::lowerConstant(CV);
}

bool JVMAsmPrinter::PrintAsmOperand(const MachineInstr *MI, unsigned OpNo,
                                    unsigned AsmVariant, const char *ExtraCode,
                                    raw_ostream &OS) {
  if (AsmVariant != 0)
    report_fatal_error("There are no defined alternate asm variants");

  // First try the generic code, which knows about modifiers like 'c' and 'n'.
  if (!AsmPrinter::PrintAsmOperand(MI, OpNo, AsmVariant, ExtraCode, OS))
    return false;

  if (!ExtraCode) {
    const MachineOperand &MO = MI->getOperand(OpNo);
    switch (MO.getType()) {
    case MachineOperand::MO_Immediate:
      OS << MO.getImm();
      return false;
    case MachineOperand::MO_MachineBasicBlock:
    case MachineOperand::MO_Register:
    case MachineOperand::MO_GlobalAddress:
    case MachineOperand::MO_ExternalSymbol:
      assert(0 && "Incorrect argument for ASM printing.");
      return false;
    default:
      break;
    }
  }

  return true;
}

void JVMAsmPrinter::EmitGlobalVariable(const GlobalVariable *GV) {
  assert(GV->isConstant() && "Non-constant globals not supported");
}

// Force static initialization.
extern "C" void LLVMInitializeJVMAsmPrinter() {
  RegisterAsmPrinter<JVMAsmPrinter> X(getTheJVMTarget32());
  RegisterAsmPrinter<JVMAsmPrinter> Y(getTheJVMTarget64());
}
