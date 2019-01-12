// JVMMachineFunctionInfo.h-JVM machine function info-*- C++ -*-
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
///
/// \file
/// \brief This file declares JVM-specific per-machine-function
/// information.
///
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_JVM_JVMMACHINEFUNCTIONINFO_H
#define LLVM_LIB_TARGET_JVM_JVMMACHINEFUNCTIONINFO_H

#include "MCTargetDesc/JVMMCTargetDesc.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/DenseSet.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/CodeGen/MachineOperand.h"
#include "llvm/CodeGen/MachineRegisterInfo.h"
#include "llvm/CodeGen/SelectionDAGNodes.h"
#include "llvm/CodeGen/ValueTypes.h"

namespace llvm {

/// This class is derived from MachineFunctionInfo and contains private
/// JVM-specific information for each MachineFunction.
class JVMFunctionInfo final : public MachineFunctionInfo {
public:
  // Map between machine virtual register and its allocated offset.
  using VRegOffsetMap = DenseMap<unsigned, int>;
  // Maps between structure type and name, this is used to generate
  // class definitions for corresponding structure types.
  using TypeNameMap = DenseMap<Type *, std::string *>;
  using NameTypeMap = DenseMap<StringRef, Type *>;
  // Map between composite nodes (STRUCT_REF/ARRAY_REF)
  // and corresponding types. Primarily used for descriptor
  // generation.
  using SDNodeTypeMap = DenseMap<SDNode *, Type *>;
  // Used to fetch the ID for a given descriptor. This ID is later
  // used to fetch the descriptor from vector of descriptors.
  using DescriptorIDMap = DenseMap<StringRef, unsigned>;
  using DescriptorVec = SmallVector<StringRef, 8>;
  // Set of special reg operand which needs to be allocated
  // e.g. allocation to virtual register operand of STORE
  // generated during COPY elision.
  using ScratchAllocationSet = DenseSet<unsigned>;
  using ScratchAllocationSetIter = DenseSet<unsigned>::Iterator;

  explicit JVMFunctionInfo(MachineFunction &MF) : MF(MF) { Initialize();}
  ~JVMFunctionInfo() override;

  void setStackLimit(unsigned limit) { StackLimit = limit; }
  void setLocalsLimit(unsigned limit) { LocalsLimit = limit; }
  unsigned getStackLimit() const { return StackLimit; }
  unsigned getLocalsLimit() const { return LocalsLimit; }

  bool isRegisterOffsetAllocated(unsigned Reg);
  int getRegisterOffset(unsigned Reg);
  void setRegisterOffset(unsigned Reg);
  void setRegisterOffset(unsigned Reg, int Offset);

  void addToScratchAllocationSet(unsigned Reg);
  ScratchAllocationSet &getScratchAllocationSet() { return scratchAllocations; }
  void clearScratchAllocationSet() { scratchAllocations.clear(); }

  unsigned getNumArguments() { return NumArguments; }
  void incrimentArgumentCount() { NumArguments++; }

  void addToNodeTypeMap(SDNode *N, Type *Ty);
  Type *getFromNodeTypeMap(SDNode *N);

  void addToTypeNameMap(Type *Ty);
  void addToTypeNameMap(Type *Ty, std::string name); 

  std::string getFromTypeNameMap(Type *Ty);
  Type * getFromTypeNameMap(StringRef & Name);

  unsigned addToDescriptorIDMap(StringRef Desc);
  StringRef getFromDescriptorVector(unsigned ID);

  TypeNameMap *getTypeNameMap() { return &STyName; }
  DescriptorVec *getDescVector() { return &DescVec; }

  void getTypeDescriptor(Type *Ty, std::string &Desc) const;

  void setEmitGlobConsts(bool Val = true) { EmitGlobConsts = Val; }
  bool IsEmitGlobConsts() const { return EmitGlobConsts; }

  void addToGlobalStructFieldInitMap(unsigned Index, Value *);
  Value * getFormGlobalStructFieldInitMap(unsigned Index);

private:
  void Initialize();

  bool EmitGlobConsts;
  unsigned NumArguments;
  unsigned StackLimit;
  unsigned LocalsLimit;

  SmallVector<MVT, 4> VTs;

  // Following fields used for composite handling to be flushed
  // after processing each basic block.
  TypeNameMap STyName;
  NameTypeMap NameSTy;

  SDNodeTypeMap NodeTypeMap;

  // Following fields needs to be preserved till ASM generation.
  DescriptorIDMap DescIDMap;
  DescriptorVec DescVec;

  VRegOffsetMap MOOMap;
  ScratchAllocationSet scratchAllocations;
  MachineFunction &MF;
};

void ComputeLegalVTs(const Function &F, const TargetMachine &TM, Type *Ty,
                     SmallVectorImpl<MVT> &ValueVTs);

void ComputeSigVTs(const Function &F, const TargetMachine &TM,
                   SmallVectorImpl<MVT> &Params, SmallVectorImpl<MVT> &Results);
} // end namespace llvm

#endif
