//- JVMISelLowering.h - JVM DAG Lowering Interface -*- C++ -*-//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
///
/// \file
/// \brief This file defines the interfaces that JVM uses to lower LLVM
/// code into a selection DAG.
///
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_JVM_JVMISELLOWERING_H
#define LLVM_LIB_TARGET_JVM_JVMISELLOWERING_H

#include "llvm/ADT/DenseMap.h"
#include "llvm/CodeGen/TargetLowering.h"

namespace llvm {

namespace JVMISD {

enum NodeType {
  FIRST_NUMBER = ISD::BUILTIN_OP_END,
  ADD,
  AND,
  ALLOC,
  ARGUMENT,
  BR_CC,
  BR_CONST_CC,
  CALL,
  CMP,
  DIV,
  DUP,
  EXTEND,
  FIELD_ACC_R,
  FIELD_ACC_W,
  INDEX_ACC,
  INVOKENV,
  LOAD,
  LOAD_W_CHAIN,
  MUL,
  OR,
  REM,
  RET,
  SHL,
  SRA,
  SRL,
  SUB,
  STORE,
  STORENR,
  TRUNC,
  XOR
};
} // end namespace JVMISD

class JVMSubtarget;
class JVMTargetMachine;

class JVMTargetLowering final : public TargetLowering {
public:
  using ReplicationMap = DenseMap<SDNode *, unsigned>;

  JVMTargetLowering(const TargetMachine &TM, const JVMSubtarget &STI);

private:
  void finalizeLowering(MachineFunction &MF) const override;

  const char *getTargetNodeName(unsigned Opcode) const override;

  bool doNotCSE(unsigned Opcode) const override;

  bool doNotPerformImplicitStaticAllocation() const override { return true; }

  bool isSuitableForJumpTable(const SwitchInst *SI, uint64_t NumCases,
                              uint64_t Range) const override { return false;}

  bool generateCompositeMacros() const override { return true; }

  SDValue createCompositeMacros(SelectionDAGBuilder *SDB, const User &I,
                                const SDLoc &dl) const override;

  bool generateAllocationMacros() const { return true; }

  SDValue createAllocationMacros(SelectionDAGBuilder *SDB, const User &I,
                                 const SDLoc &dl) const;

  int getNodeReplicatorCounter(unsigned Opcode, SDNode *N = nullptr) override;

  void addNodeToReplicationMap(SDNode *N) override;

  bool checkIfAbsorptionCandidate(SDValue V) const override;

  void clearAuxiliaryData() override;

  bool skipArgumentTypeValidation(bool IsComposite) const {
    return IsComposite;
  }

  bool skipDAGCombining(unsigned Opcode) const override;

  bool CanLowerReturn(CallingConv::ID CallConv, MachineFunction &MF,
                      bool isVarArg,
                      const SmallVectorImpl<ISD::OutputArg> &Outs,
                      LLVMContext &Context) const override;

  SDValue LowerCall(CallLoweringInfo &CLI,
                    SmallVectorImpl<SDValue> &InVals) const override;

  SDValue LowerReturn(SDValue Chain, CallingConv::ID CallConv, bool isVarArg,
                      const SmallVectorImpl<ISD::OutputArg> &Outs,
                      const SmallVectorImpl<SDValue> &OutVals, const SDLoc &dl,
                      SelectionDAG &DAG) const override;

  SDValue LowerFormalArguments(SDValue Chain, CallingConv::ID CallConv,
                               bool IsVarArg,
                               const SmallVectorImpl<ISD::InputArg> &Ins,
                               const SDLoc &DL, SelectionDAG &DAG,
                               SmallVectorImpl<SDValue> &InVals) const override;

  // Custom lowering hooks.
  SDValue LowerOperation(SDValue Op, SelectionDAG &DAG) const override;

  // Custom DAG combiner.
  SDValue PerformDAGCombine(SDNode *N, DAGCombinerInfo &DCI) const override;

  /// Keep a pointer to the JVMSubtarget around so that we can make the
  /// right decision when generating code for different targets.
  const JVMSubtarget *Subtarget;
  int Replicator;
  ReplicationMap RepMap;
  bool IsArch64bit;
};
} // end namespace llvm

#endif
