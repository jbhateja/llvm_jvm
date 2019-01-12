//=- JVMMachineFunctionInfo.cpp - JVM Machine Function Info -=//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
///
/// \file
/// \brief This file implements JVM-specific per-machine-function
/// information.
///
//===----------------------------------------------------------------------===//

#include "JVMMachineFunctionInfo.h"
#include "JVMISelLowering.h"
#include "JVMSubtarget.h"
#include "llvm/CodeGen/Analysis.h"
#include "llvm/Support/Debug.h"

#define DEBUG_TYPE "jvm-func-info"

using namespace llvm;

void llvm::ComputeLegalVTs(const Function &F, const TargetMachine &TM, Type *Ty,
                           SmallVectorImpl<MVT> &ValueVTs) {
  const DataLayout &DL(F.getParent()->getDataLayout());
  const JVMTargetLowering &TLI =
      *TM.getSubtarget<JVMSubtarget>(F).getTargetLowering();
  SmallVector<EVT, 4> VTs;
  ComputeValueVTs(TLI, DL, Ty, VTs);

  for (EVT VT : VTs) {
    unsigned NumRegs = TLI.getNumRegisters(F.getContext(), VT);
    MVT RegisterVT = TLI.getRegisterType(F.getContext(), VT);
    for (unsigned i = 0; i != NumRegs; ++i)
      ValueVTs.push_back(RegisterVT);
  }
}

void llvm::ComputeSigVTs(const Function &F, const TargetMachine &TM,
                         SmallVectorImpl<MVT> &Params,
                         SmallVectorImpl<MVT> &Results) {
  ComputeLegalVTs(F, TM, F.getReturnType(), Results);

  if (Results.size() > 1) {
    // JVM currently can't lower returns of multiple values without
    // demoting to sret (see JVMTargetLowering::CanLowerReturn). So
    // replace multiple return values with a pointer parameter.
    Results.clear();
    Params.push_back(
        MVT::getIntegerVT(TM.createDataLayout().getPointerSizeInBits()));
  }

  for (auto &Arg : F.args())
    ComputeLegalVTs(F, TM, Arg.getType(), Params);
}

static std::string *GenClassName() {
  static unsigned ClassNum = 1;
  char buff[128] = {'\0'};
  std::string *Name = new std::string("AoCClass");
  sprintf(buff, "%d", ClassNum++);
  *Name += buff;
  return Name;
}

JVMFunctionInfo::~JVMFunctionInfo() {}

bool JVMFunctionInfo::isRegisterOffsetAllocated(unsigned Reg) {
  return MOOMap.size() && MOOMap.find(Reg) != MOOMap.end();
}

int JVMFunctionInfo::getRegisterOffset(unsigned Reg) {
  assert(MOOMap.size() && MOOMap.find(Reg) != MOOMap.end() &&
         "Missing virtual register offset.");
  return MOOMap[Reg];
}

void JVMFunctionInfo::setRegisterOffset(unsigned Reg, int Offset) {
  assert(MOOMap.find(Reg) == MOOMap.end() &&
         "Offset already allocated to virutal register.");
  MOOMap[Reg] = Offset;
}

void JVMFunctionInfo::setRegisterOffset(unsigned Reg) {
  MachineRegisterInfo &MRI = MF.getRegInfo();
  const TargetRegisterClass *RC = MRI.getRegClass(Reg);
  setRegisterOffset(Reg, LocalsLimit);
  if (RC == &JVM::I64RegClass || RC == &JVM::F64RegClass)
    LocalsLimit += 2;
  else
    LocalsLimit += 1;
}

Type * JVMFunctionInfo::getFromTypeNameMap(StringRef &name) {
  assert(NameSTy.find(name) != NameSTy.end());
  return NameSTy[name];
}

std::string JVMFunctionInfo::getFromTypeNameMap(Type *Ty) {
  assert(STyName.find(Ty) != STyName.end());
  return *STyName[Ty];
}

void JVMFunctionInfo::addToTypeNameMap(Type *Ty, std::string name) {
  std::string * N = new std::string(name); 
  STyName[Ty] = N;
  NameSTy[*N] = Ty;
}

void JVMFunctionInfo::addToTypeNameMap(Type *Ty) {
  assert(isa<StructType>(Ty));
  if (STyName.find(Ty) == STyName.end()) {
    std::string * Name = GenClassName();
    STyName[Ty] = Name;
    NameSTy[*Name] = Ty;
  }
}

void JVMFunctionInfo::addToNodeTypeMap(SDNode *N, Type *Ty) {
  if (NodeTypeMap.find(N) == NodeTypeMap.end())
    NodeTypeMap[N] = Ty;
}

Type *JVMFunctionInfo::getFromNodeTypeMap(SDNode *N) {
  if (NodeTypeMap.find(N) != NodeTypeMap.end())
    return NodeTypeMap[N];
  return nullptr;
}

void JVMFunctionInfo::addToScratchAllocationSet(unsigned Reg) {
  if (scratchAllocations.find(Reg) == scratchAllocations.end()) {
    DEBUG(dbgs() << "Adding Reg = " << Reg << " to StratchAllocationSet\n");
    scratchAllocations.insert(Reg);
  }
}

void JVMFunctionInfo::getTypeDescriptor(Type *Ty, std::string &Desc) const {
  JVMFunctionInfo *MFI = const_cast<JVMFunctionInfo *>(this);
  const DataLayout &DL = MF.getDataLayout();
  const JVMTargetLowering &TLI =
      *MF.getTarget()
           .getSubtarget<JVMSubtarget>(MF.getFunction())
           .getTargetLowering();

  if (isa<ArrayType>(Ty)) {
    Desc += "[";
    getTypeDescriptor(cast<SequentialType>(Ty)->getElementType(), Desc);
  } else if (isa<PointerType>(Ty)) {
    getTypeDescriptor(cast<PointerType>(Ty)->getElementType(), Desc);
  } else if (isa<StructType>(Ty)) {
    Desc += "L";
    Desc += MFI->getFromTypeNameMap(Ty);
    Desc += ";";
  } else if (Ty->isVoidTy()) {
    Desc += "V";
  } else if (Ty->isSingleValueType()) {
    EVT VT = TLI.getValueType(DL, Ty);
    switch (VT.getSimpleVT().SimpleTy) {
    case MVT::i8:
      Desc += "B";
      break;
    case MVT::i16:
      Desc += "S";
      break;
    case MVT::i32:
      Desc += "I";
      break;
    case MVT::i64:
      Desc += "J";
      break;
    case MVT::i1:
      Desc += 'Z';
      break;
    default:
      llvm_unreachable("Unhandled type in GetTypeDescriptor()");
    }
  } else {
    llvm_unreachable("Unhandled type in GetTypeDescriptor()");
  }
}

unsigned JVMFunctionInfo::addToDescriptorIDMap(StringRef Desc) {
  if (DescIDMap.find(Desc) == DescIDMap.end()) {
    int ID = DescVec.size();
    // TODO : Cleanup of desc string vector post its usage in InstPrinter.
    std::string *DescStr = new std::string(Desc.data());
    StringRef D(DescStr->c_str());
    DescVec.push_back(D);
    DescIDMap[Desc] = ID;
  }
  return DescIDMap[Desc];
}

StringRef JVMFunctionInfo::getFromDescriptorVector(unsigned ID) {
  assert(DescIDMap.find(DescVec[ID]) != DescIDMap.end());
  return DescVec[ID];
}

void JVMFunctionInfo::Initialize() {    
  EmitGlobConsts = false;
  NumArguments = 0;
  StackLimit = 0;
  LocalsLimit = 0;
}

