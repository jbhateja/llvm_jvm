//=- JVMISelLowering.cpp - JVM DAG Lowering Implementation -==//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
///
/// \file
/// \brief This file implements the JVMTargetLowering class.
///
//===----------------------------------------------------------------------===//

#include "JVMISelLowering.h"
#include "JVMMachineFunctionInfo.h"
#include "JVMSubtarget.h"
#include "JVMTargetMachine.h"
#include "MCTargetDesc/JVMMCTargetDesc.h"
#include "llvm/CodeGen/Analysis.h"
#include "llvm/CodeGen/CallingConvLower.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/MachineJumpTableInfo.h"
#include "llvm/CodeGen/MachineRegisterInfo.h"
#include "llvm/CodeGen/SelectionDAG.h"
#include "llvm/CodeGen/SelectionDAGBuilder.h"
#include "llvm/CodeGen/SelectionDAGISel.h"
#include "llvm/CodeGen/SelectionDAGNodes.h"
#include "llvm/IR/DiagnosticInfo.h"
#include "llvm/IR/DiagnosticPrinter.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/GetElementPtrTypeIterator.h"
#include "llvm/IR/Intrinsics.h"
#include "llvm/IR/Metadata.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Target/TargetOptions.h"
using namespace llvm;

#define DEBUG_TYPE "jvm-lower"
#define IS_START_OF_CHAIN(X) (X & 0x1)
#define IS_MIDDLE_OF_CHAIN(X) ((X & (0x1 << 1)) && !(X & 0x1))
#define IS_END_OF_CHAIN(X) (X & (0x1 << 2))

static SDValue LoadOperand(SelectionDAG &DAG, const SDLoc &DL, EVT VT,
                           SDValue Val);

JVMTargetLowering::JVMTargetLowering(const TargetMachine &TM,
                                     const JVMSubtarget &STI)
    : TargetLowering(TM), Subtarget(&STI) {

  // We don't know the microarchitecture here, so just reduce register pressure.
  setSchedulingPreference(Sched::RegPressure);

  // Set up the register classes.
  addRegisterClass(MVT::i8, &JVM::I8RegClass);
  addRegisterClass(MVT::i16, &JVM::I16RegClass);
  addRegisterClass(MVT::i32, &JVM::I32RegClass);
  addRegisterClass(MVT::i64, &JVM::I64RegClass);
  addRegisterClass(MVT::f32, &JVM::F32RegClass);
  addRegisterClass(MVT::f64, &JVM::F64RegClass);

  // Compute derived properties from the register classes.
  computeRegisterProperties(Subtarget->getRegisterInfo());

  // TODO : Pull out operations requiring custom implementation.
  for (auto T : {MVT::i32, MVT::i64})
    for (unsigned Opc = 0; Opc < ISD::BUILTIN_OP_END; ++Opc)
      setOperationAction(Opc, T, Legal);

  for (auto T : {MVT::i8, MVT::i16, MVT::i32, MVT::f32, MVT::i64, MVT::f64}) {
    setOperationAction(ISD::OR, T, Custom);
    setOperationAction(ISD::SHL, T, Custom);
    setOperationAction(ISD::SRL, T, Custom);
    setOperationAction(ISD::SRA, T, Custom);
    setOperationAction(ISD::XOR, T, Custom);
    setOperationAction(ISD::AND, T, Custom);
    setOperationAction(ISD::ADD, T, Custom);
    setOperationAction(ISD::UDIV, T, Custom);
    setOperationAction(ISD::SDIV, T, Custom);
    setOperationAction(ISD::UREM, T, Custom);
    setOperationAction(ISD::SREM, T, Custom);
    setOperationAction(ISD::SUB, T, Custom);
    setOperationAction(ISD::MUL, T, Custom);
    setOperationAction(ISD::BR_CC, T, Custom);
  }

  for (auto T : {MVT::i8, MVT::i16, MVT::i32, MVT::i64}) {
    setOperationAction(ISD::SIGN_EXTEND, T, Custom);
    setOperationAction(ISD::ZERO_EXTEND, T, Custom);
    setOperationAction(ISD::TRUNCATE, T, Custom);
    setOperationAction(ISD::ANY_EXTEND, T, Custom);
    setOperationAction(ISD::LOAD, T, Custom);
    setOperationAction(ISD::STORE, T, Custom);
    setOperationAction(ISD::SIGN_EXTEND_INREG, T, Custom);
  }

  setOperationAction(ISD::BRCOND, MVT::Other, Custom);
  setOperationAction(ISD::GlobalAddress, MVT::i32, Custom);
  setOperationAction(ISD::GlobalAddress, MVT::i64, Custom);

  for (auto T : {MVT::i8, MVT::i16, MVT::i32, MVT::i64}) {
    setOperationAction(ISD::DYNAMIC_STACKALLOC, T, Custom);
    setOperationAction(ISD::ARRAY_REF, T, Custom);
    setOperationAction(ISD::STRUCT_REF, T, Custom);
  }

  for (auto T : {MVT::i8, MVT::i16, MVT::i32, MVT::i64}) {
    setLoadExtAction(ISD::EXTLOAD, T, MVT::i1, Promote);
    setLoadExtAction(ISD::ZEXTLOAD, T, MVT::i1, Promote);
    setLoadExtAction(ISD::SEXTLOAD, T, MVT::i1, Promote);
  }

  Replicator = 0;
  RepMap.clear();
}

bool JVMTargetLowering::checkIfAbsorptionCandidate(SDValue V) const {
  SDNodeFlags Flags = V.getNode()->getFlags();
  return Flags.hasIsUseAbsorb();
}

const char *JVMTargetLowering::getTargetNodeName(unsigned Opcode) const {
  switch (Opcode) {
  default:
    llvm_unreachable("Unhandled target opcode.\n");
    break;
  case JVMISD::ALLOC:
    return "JVMISD::ALLOC";
  case JVMISD::ADD:
    return "JVMISD::ADD";
  case JVMISD::AND:
    return "JVMISD::AND";
  case JVMISD::DIV:
    return "JVMISD::DIV";
  case JVMISD::EXTEND:
    return "JVMISD::EXTEND";
  case JVMISD::ARGUMENT:
    return "JVMISD::ARGUMENT";
  case JVMISD::LOAD:
    return "JVMISD::LOAD";
  case JVMISD::LOAD_W_CHAIN:
    return "JVMISD::LOAD_W_CHAIN";
  case JVMISD::MUL:
    return "JVMISD::MUL";
  case JVMISD::SHL:
    return "JVMISD::SHL";
  case JVMISD::SRL:
    return "JVMISD::SRL";
  case JVMISD::SRA:
    return "JVMISD::SRA";
  case JVMISD::STORENR:
    return "JVMISD::STORENR";
  case JVMISD::STORE:
    return "JVMISD::STORE";
  case JVMISD::TRUNC:
    return "JVMISD::TRUNC";
  case JVMISD::FIELD_ACC_R:
    return "JVMISD::FIELD_ACC_R";
  case JVMISD::FIELD_ACC_W:
    return "JVMISD::FIELD_ACC_W";
  case JVMISD::INDEX_ACC:
    return "JVMISD::INDEX_ACC";
  case JVMISD::INVOKENV:
    return "JVMISD::INVOKENV";
  case JVMISD::OR:
    return "JVMISD::OR";
  case JVMISD::SUB:
    return "JVMISD::SUB";
  case JVMISD::CMP:
    return "JVMISD::CMP";
  case JVMISD::CALL:
    return "JVMISD::CALL";
  case JVMISD::REM:
    return "JVMISD::REM";
  case JVMISD::RET:
    return "JVMISD::RET";
  case JVMISD::DUP:
    return "JVMISD::DUP";
  case JVMISD::BR_CC:
    return "JVMISD::BR_CC";
  case JVMISD::BR_CONST_CC:
    return "JVMISD::BR_CONST_CC";
  case JVMISD::XOR:
    return "JVMISD::XOR";
  }
  return nullptr;
}

// Following routine is called during DAG construction phase.
SDValue JVMTargetLowering::createCompositeMacros(SelectionDAGBuilder *SDB,
                                                 const User &I,
                                                 const SDLoc &dl) const {
  using IndexKind = enum { SCALAR_REF, ARRAY_REF, STRUCT_REF };
  assert(isa<GetElementPtrInst>(&I) && "Invalid argument passed, GEP expected");

  SelectionDAG &DAG = SDB->DAG;
  Value *PtrVal = I.getOperand(0);
  SDValue N = SDB->getValue(PtrVal);
  IndexKind NextIndex = SCALAR_REF;
  const DataLayout &DL = DAG.getDataLayout();

  JVMFunctionInfo *MFI = DAG.getMachineFunction().getInfo<JVMFunctionInfo>();
  PointerType *MemPtr = cast<PointerType>(PtrVal->getType());
  assert(isa<StructType>(MemPtr->getElementType()) ||
         isa<ArrayType>(MemPtr->getElementType()));

  // End of chain user (LOAD/STORE) are not processed by DAG lowering
  // pass since they will be absorbed during composite lowering, remaining
  // LOAD/STORE will be lowered in usual manner.
  int NumIndices = cast<GetElementPtrInst>(&I)->getNumIndices();
  int IsInChain = !isa<GetElementPtrInst>(PtrVal);
  for (gep_type_iterator GTI = gep_type_begin(&I), E = gep_type_end(&I);
       GTI != E; ++GTI) {
    NumIndices--;
    const Value *Idx = GTI.getOperand();
    Type *IdxTy = GTI.getIndexedType();
    if (NextIndex == ARRAY_REF || NextIndex == STRUCT_REF) {
      unsigned NodeOpcode =
          NextIndex == STRUCT_REF ? ISD::STRUCT_REF : ISD::ARRAY_REF;
      IsInChain |= 0x1 << 1;
      IsInChain |= (0 == NumIndices) << 2;
      SDValue InChainUse = DAG.getConstant(IsInChain, dl, MVT::i32);
      IsInChain &= ~0x1;
      // STRUCT_REF/ARRAY_REF <CHAIN> <OBJ> <INDEX> <IN/OUT CHAIN>
      SDValue Ops[] = {SDB->getRoot(), N, SDB->getValue(Idx), InChainUse};
      if (isa<CompositeType>(IdxTy)) {
        EVT VTs[] = {MVT::i32, MVT::Other};
        N = DAG.getNode(NodeOpcode, dl, VTs, Ops);
        // Setting IsComposite property over DAG node distinguishes it from
        // scalar Node having same valuetypes.
        SDNodeFlags Flags = N.getNode()->getFlags();
        Flags.setIsComposite(true);
        N.getNode()->setFlags(Flags);
      } else {
        EVT IdxVT = getValueType(DL, IdxTy);
        EVT VTs[] = {IdxVT, MVT::Other};
        N = DAG.getNode(NodeOpcode, dl, VTs, Ops);
      }
      MFI->addToNodeTypeMap(N.getNode(), IdxTy);

      SDNodeFlags Flags = N.getNode()->getFlags();
      // Last composite reference absorbs its LOAD/STORE user.
      if (IS_END_OF_CHAIN(IsInChain))
        Flags.setIsUseAbsorb(true);
      // Mark IsStructAcc/IsArrayAcc flags over node, these
      // guide instruction selection process.
      if (NextIndex == STRUCT_REF)
        Flags.setIsStructAcc(true);
      else
        Flags.setIsArrayAcc(true);
      N.getNode()->setFlags(Flags);
      DAG.setRoot(N.getValue(1));
    }

    if (isa<StructType>(IdxTy)) {
      NextIndex = STRUCT_REF;
      // Generate a class name corresponding to structure type.
      MFI->addToTypeNameMap(IdxTy);
      // Following mapping used to generate descriptors while DAG Lowering.
      MFI->addToNodeTypeMap(N.getNode(), IdxTy);
    } else if (isa<ArrayType>(IdxTy)) {
      NextIndex = ARRAY_REF;
    } else {
      NextIndex = SCALAR_REF;
    }
  }
  return N;
}

SDValue JVMTargetLowering::createAllocationMacros(SelectionDAGBuilder *SDB,
                                                  const User &I,
                                                  const SDLoc &DL) const {
  SelectionDAG &DAG = SDB->DAG;
  JVMFunctionInfo *MFI = DAG.getMachineFunction().getInfo<JVMFunctionInfo>();
  EVT CompVT(MVT::i32);

  const AllocaInst *AI = cast<const AllocaInst>(&I);
  SDValue ArrSz = SDB->getValue(AI->getArraySize());
  EVT VTs[] = {CompVT, MVT::Other};
  SDValue Ops[] = {SDB->getRoot(), ArrSz, DAG.getConstant(4, DL, MVT::i32)};
  SDValue DSA = DAG.getNode(ISD::DYNAMIC_STACKALLOC, DL, VTs, Ops);

  Type *AllocatedTy = AI->getAllocatedType();
  MFI->addToNodeTypeMap(DSA.getNode(), AllocatedTy);
  if (isa<StructType>(AllocatedTy))
    MFI->addToTypeNameMap(AllocatedTy);
  DAG.setRoot(DSA.getValue(1));
  return DSA;
}

//===----------------------------------------------------------------------===//
// JVM Lowering private implementation.
//===----------------------------------------------------------------------===//

//===----------------------------------------------------------------------===//
// Lowering Code
//===----------------------------------------------------------------------===//

static void fail(const SDLoc &DL, SelectionDAG &DAG, const char *msg) {
  MachineFunction &MF = DAG.getMachineFunction();
  DAG.getContext()->diagnose(
      DiagnosticInfoUnsupported(MF.getFunction(), msg, DL.getDebugLoc()));
}

// Test whether the given calling convention is supported.
// JVM supports only C-calling convention.
static bool CallingConvSupported(CallingConv::ID CallConv) {
  return CallConv == CallingConv::C;
}

void JVMTargetLowering::finalizeLowering(MachineFunction &MF) const {
  // TODO: Finalize any post lowering steps.
  auto &MRI = MF.getRegInfo();
  MRI.freezeReservedRegs(MF);
}

bool JVMTargetLowering::doNotCSE(unsigned Opcode) const {
  switch (Opcode) {
  default:
    return false;
  case ISD::STRUCT_REF:
  case ISD::ARRAY_REF:
  case JVMISD::LOAD:
  case JVMISD::LOAD_W_CHAIN:
  case JVMISD::STORE:
  case JVMISD::STORENR:
  case JVMISD::FIELD_ACC_W:
  case JVMISD::FIELD_ACC_R:
  case JVMISD::INDEX_ACC:
  case JVMISD::INVOKENV:
  case JVMISD::DUP:
    return true;
  }
}

bool JVMTargetLowering::skipDAGCombining(unsigned Opcode) const {
  switch (Opcode) {
  default:
    return false;
  case ISD::OR:
  case ISD::AND:
  case ISD::LOAD:
  case ISD::STORE:
  case ISD::SIGN_EXTEND:
  case ISD::ZERO_EXTEND:
  case ISD::ANY_EXTEND:
  case ISD::SHL:
  case ISD::SRL:
  case ISD::TRUNCATE:
  case ISD::SREM:
    return true;
  }
}

// TODO: Return node specific replicator counter.
int JVMTargetLowering::getNodeReplicatorCounter(unsigned Opcode, SDNode *N) {
  assert(doNotCSE(Opcode) &&
         "Cannot return replicator counter for nodes to be shared.");
  if (N && RepMap.find(N) != RepMap.end())
    return RepMap[N];

  return Replicator++;
}

void JVMTargetLowering::addNodeToReplicationMap(SDNode *N) {
  assert(doNotCSE(N->getOpcode()) && "Cannot add node to replicator map.");
  if (RepMap.find(N) == RepMap.end()) {
    RepMap[N] = Replicator - 1;
  }
}

SDValue JVMTargetLowering::LowerCall(CallLoweringInfo &CLI,
                                     SmallVectorImpl<SDValue> &InVals) const {
  SelectionDAG &DAG = CLI.DAG;
  const SDLoc DL = CLI.DL;
  SDValue Chain = CLI.Chain;
  SDValue Callee = CLI.Callee;
  MachineFunction &MF = DAG.getMachineFunction();
  auto Layout = MF.getDataLayout();

  CallingConv::ID CallConv = CLI.CallConv;
  if (!CallingConvSupported(CallConv))
    fail(DL, DAG,
         "JVM doesn't support language-specific or target-specific "
         "calling conventions yet");
  if (CLI.IsPatchPoint)
    fail(DL, DAG, "JVM doesn't support patch point yet");

  SmallVectorImpl<ISD::InputArg> &Ins = CLI.Ins;
  if (Ins.size() > 1)
    fail(DL, DAG, "JVM doesn't support more than 1 returned value yet");

  SmallVectorImpl<ISD::OutputArg> &Outs = CLI.Outs;
  SmallVectorImpl<SDValue> &OutVals = CLI.OutVals;
  for (unsigned i = 0; i < Outs.size(); ++i) {
    const ISD::OutputArg &Out = Outs[i];
    SDValue &OutVal = OutVals[i];
    EVT VT = OutVal.getValueType();
    if (Out.Flags.isNest())
      fail(DL, DAG, "JVM hasn't implemented nest arguments");
    if (Out.Flags.isInAlloca())
      fail(DL, DAG, "JVM hasn't implemented inalloca arguments");
    if (Out.Flags.isInConsecutiveRegs())
      fail(DL, DAG, "JVM hasn't implemented cons regs arguments");
    if (Out.Flags.isInConsecutiveRegsLast())
      fail(DL, DAG, "JVM hasn't implemented cons regs last arguments");
    if (Out.Flags.isByVal() && Out.Flags.getByValSize() != 0)
      Chain = DAG.getNode(JVMISD::LOAD, DL, VT, OutVal);
  }

  bool IsVarArg = CLI.IsVarArg;
  unsigned NumFixedArgs = CLI.NumFixedArgs;

  // Analyze operands of the call, assigning locations to each operand.
  SmallVector<CCValAssign, 16> ArgLocs;
  CCState CCInfo(CallConv, IsVarArg, MF, ArgLocs, *DAG.getContext());

  if (IsVarArg) {
    // Outgoing non-fixed arguments are placed in a buffer. First
    // compute their offsets and the total amount of buffer space needed.
    for (SDValue Arg :
         make_range(OutVals.begin() + NumFixedArgs, OutVals.end())) {
      EVT VT = Arg.getValueType();
      assert(VT != MVT::iPTR && "Legalized args should be concrete");
      Type *Ty = VT.getTypeForEVT(*DAG.getContext());
      unsigned Offset = CCInfo.AllocateStack(Layout.getTypeAllocSize(Ty),
                                             Layout.getABITypeAlignment(Ty));
      CCInfo.addLoc(CCValAssign::getMem(ArgLocs.size(), VT.getSimpleVT(),
                                        Offset, VT.getSimpleVT(),
                                        CCValAssign::Full));
    }
  }

  if (IsVarArg)
    llvm_unreachable("Unsupported variable arguments call.");

  // Compute the operands for the CALL node.
  SmallVector<SDValue, 16> Ops;
  Ops.push_back(Chain);
  Ops.push_back(Callee);

  SmallVector<EVT, 8> InTys;
  for (const auto &In : Ins) {
    assert(!In.Flags.isByVal() && "byval is not valid for return values");
    assert(!In.Flags.isNest() && "nest is not valid for return values");
    if (In.Flags.isInAlloca())
      fail(DL, DAG, "JVM hasn't implemented inalloca return values");
    if (In.Flags.isInConsecutiveRegs())
      fail(DL, DAG, "JVM hasn't implemented cons regs return values");
    if (In.Flags.isInConsecutiveRegsLast())
      fail(DL, DAG, "JVM hasn't implemented cons regs last return values");
    // Ignore In.getOrigAlign() because all our arguments are passed in
    // registers.
    InTys.push_back(In.VT);
  }
  InTys.push_back(MVT::Other);
  SDVTList InTyList = DAG.getVTList(InTys);
  SDValue Res = DAG.getNode(JVMISD::CALL, DL, InTyList, Ops);

  if (Ins.empty()) {
    Chain = Res;
  } else {
    InVals.push_back(Res);
    Chain = Res.getValue(1);
  }

  return Chain;
}

bool JVMTargetLowering::CanLowerReturn(
    CallingConv::ID /*CallConv*/, MachineFunction & /*MF*/, bool /*IsVarArg*/,
    const SmallVectorImpl<ISD::OutputArg> &Outs,
    LLVMContext & /*Context*/) const {
  // JVM can't handle returning tuples.
  return Outs.size() <= 1;
}

SDValue
JVMTargetLowering::LowerReturn(SDValue Chain, CallingConv::ID CallConv,
                               bool /*IsVarArg*/,
                               const SmallVectorImpl<ISD::OutputArg> &Outs,
                               const SmallVectorImpl<SDValue> &OutVals,
                               const SDLoc &DL, SelectionDAG &DAG) const {

  assert(Outs.size() <= 1 && "JVM can only return up to one value");

  if (!CallingConvSupported(CallConv))
    fail(DL, DAG, "JVM doesn't support non-C calling conventions");

  if (Outs.size())
    Chain = DAG.getNode(JVMISD::RET, DL, MVT::Other, Chain, OutVals[0]);
  else
    Chain = DAG.getNode(JVMISD::RET, DL, MVT::Other, Chain);


  // Record the number and types of the return values.
  for (const ISD::OutputArg &Out : Outs) {
    assert(!Out.Flags.isByVal() && "byval is not valid for return values");
    assert(!Out.Flags.isNest() && "nest is not valid for return values");
    assert(Out.IsFixed && "non-fixed return value is not valid");
    if (Out.Flags.isInAlloca())
      fail(DL, DAG, "JVM hasn't implemented inalloca results");
    if (Out.Flags.isInConsecutiveRegs())
      fail(DL, DAG, "JVM hasn't implemented cons regs results");
    if (Out.Flags.isInConsecutiveRegsLast())
      fail(DL, DAG, "JVM hasn't implemented cons regs last results");
  }

  return Chain;
}

SDValue JVMTargetLowering::LowerFormalArguments(
    SDValue Chain, CallingConv::ID CallConv, bool IsVarArg,
    const SmallVectorImpl<ISD::InputArg> &Ins, const SDLoc &DL,
    SelectionDAG &DAG, SmallVectorImpl<SDValue> &InVals) const {
  if (!CallingConvSupported(CallConv))
    fail(DL, DAG, "JVM doesn't support non-C calling conventions");

  MachineFunction &MF = DAG.getMachineFunction();
  JVMFunctionInfo *MFI = MF.getInfo<JVMFunctionInfo>();

  // Set up the incoming ARGUMENTS value, which serves to represent the liveness
  // of the incoming values before they're represented by virtual registers.
  MF.getRegInfo().addLiveIn(JVM::ARGUMENTS);

  for (const ISD::InputArg &In : Ins) {
    if (In.Flags.isInAlloca())
      fail(DL, DAG, "JVM hasn't implemented inalloca arguments");
    if (In.Flags.isNest())
      fail(DL, DAG, "JVM hasn't implemented nest arguments");
    if (In.Flags.isInConsecutiveRegs())
      fail(DL, DAG, "JVM hasn't implemented cons regs arguments");
    if (In.Flags.isInConsecutiveRegsLast())
      fail(DL, DAG, "JVM hasn't implemented cons regs last arguments");
    // Ignore In.getOrigAlign() because all our arguments are passed in
    // registers.
    SDValue Arg;
    if (In.Used) {
      bool IsCompositeArg = In.Flags.isByVal();
      EVT ArgTy = IsCompositeArg ? MVT::i32 : In.VT;
      Arg = DAG.getNode(JVMISD::ARGUMENT, DL, ArgTy,
                        DAG.getTargetConstant(InVals.size(), DL, MVT::i32));
      SDNodeFlags Flags = Arg.getNode()->getFlags();
      Flags.setIsComposite(IsCompositeArg);
      Arg.getNode()->setFlags(Flags);
    } else {
      Arg = DAG.getUNDEF(In.VT);
    }
    InVals.push_back(Arg);
    MFI->incrimentArgumentCount();
  }

  // Varargs are copied into a buffer allocated by the caller, and a pointer to
  // the buffer is passed as an argument.
  if (IsVarArg)
    llvm_unreachable("Unsupported variable arguments.");

  return Chain;
}

//===----------------------------------------------------------------------===//
//  Custom lowering hooks.
//===----------------------------------------------------------------------===//

static void TransferNodeFlags(SDValue Old, SDValue New) {
  SDNodeFlags Flags = Old.getNode()->getFlags();
  Flags.setGlueOperands(false);
  New.getNode()->setFlags(Flags);
}

static void TransferNodeFlagsWithGlue(SDValue Old, SDValue New) {
  SDNodeFlags Flags = Old.getNode()->getFlags();
  Flags.setGlueOperands(true);
  New.getNode()->setFlags(Flags);
}

static unsigned GetLowerBinaryOp(unsigned Opcode) {
  switch (Opcode) {
  default:
    llvm_unreachable("Unhandled binary operation while lowering.");
    break;
  case ISD::ADD:
    return JVMISD::ADD;
  case ISD::SUB:
    return JVMISD::SUB;
  case ISD::MUL:
    return JVMISD::MUL;
  case ISD::OR:
    return JVMISD::OR;
  case ISD::XOR:
    return JVMISD::XOR;
  case ISD::AND:
    return JVMISD::AND;
  case ISD::SHL:
    return JVMISD::SHL;
  case ISD::SRL:
    return JVMISD::SRL;
  case ISD::SRA:
    return JVMISD::SRA;
  case ISD::SDIV:
  case ISD::UDIV:
    return JVMISD::DIV;
  case ISD::UREM:
  case ISD::SREM:
    return JVMISD::REM;
  }
}

static unsigned GetLoadOpcode(EVT VT, bool withChain = false) {
  if (VT.isInteger() || VT.isFloatingPoint())
    return withChain ? JVMISD::LOAD_W_CHAIN : JVMISD::LOAD;
  else
    llvm_unreachable("Unhandled load type while lowering.");
  return ISD::LOAD;
}

bool IsLoadLike(unsigned Opcode) {
  switch (Opcode) {
  default:
    return false;
  case ISD::ANY_EXTEND:
  case ISD::SIGN_EXTEND:
  case ISD::ZERO_EXTEND:
    return true;
  }
}

bool IsLoadOperand(SDValue Opr) {
  unsigned Opcode = Opr.getOpcode();
  switch (Opcode) {
  default:
    return false;
  case ISD::LOAD: {
    SDValue Ptr = Opr.getOperand(1);
    return !Ptr.getNode()->getFlags().hasIsAbsorptionCandidate();
  }
  case JVMISD::LOAD:
  case JVMISD::LOAD_W_CHAIN:
    return true;
  }
}

static SDValue LoadOperand(SelectionDAG &DAG, const SDLoc &DL, EVT VT,
                           SDValue Val) {
  SDNodeFlags Flags = Val.getNode()->getFlags();
  auto CompatibleLoad = [=](SDValue Opr, EVT VT) -> bool {
    if (IsLoadOperand(Opr) && Opr.getValueType() == VT)
      return true;
    else
      return false;
  };
  bool CanSkipF = (!Flags.hasIsAbsorptionCandidate() && 
          (CompatibleLoad(Val, VT) || IsLoadLike(Val.getOpcode())));
                   
  if (CanSkipF)
    return Val;

  unsigned LoadOpcode = GetLoadOpcode(VT);
  return DAG.getNode(LoadOpcode, DL, VT, Val);
}

static SDValue LoadOperandWCh(SelectionDAG &DAG, const SDLoc &DL,
                              ArrayRef<EVT> VTs, ArrayRef<SDValue> Vals) {
  assert(Vals[0].getValueType() == MVT::Other && VTs[1] == MVT::Other);
  SDNode *ValNode = Vals[1].getNode();
  SDNodeFlags Flags = ValNode->getFlags();

  auto CompatibleLoad = [=](SDValue Opr, EVT VT) -> bool {
    if (Opr.getOpcode() == JVMISD::LOAD_W_CHAIN && Opr.getValueType() == VT)
      return true;
    else
      return false;
  };
  bool CanSkipF =
      (!Flags.hasIsAbsorptionCandidate() && CompatibleLoad(Vals[1], VTs[0]));
  if (CanSkipF) {
    SDNode *ValNode = Vals[1].getNode();
    assert(ValNode->getValueType(ValNode->getNumValues() - 1) == MVT::Other);
    return Vals[1];
  }
  unsigned LoadOpcode = GetLoadOpcode(VTs[0], true);
  return DAG.getNode(LoadOpcode, DL, VTs, Vals);
}

static SDValue LowerBinaryOp(SDValue Op, SelectionDAG &DAG) {
  assert(Op.getNumOperands() == 2 && Op.getNode()->getNumValues() == 1 &&
         "Unhandled binary lowering.");
  SDValue BinOp;
  SDValue LoadInp1;
  SDValue LoadInp2;
  SDLoc DL(Op.getNode());
  EVT VT = Op.getValueType();
  SDValue Opr1 = Op.getOperand(0);
  SDValue Opr2 = Op.getOperand(1);

  unsigned LowerBinOp = GetLowerBinaryOp(Op.getOpcode());

  // Fundamental granularity at which JVM instructions
  // operate is an integer (i32).
  if (VT == MVT::i16 || VT == MVT::i8) {
    LoadInp1 = DAG.getNode(ISD::ZERO_EXTEND, DL, MVT::i32, Opr1);
    LoadInp2 = DAG.getNode(ISD::ZERO_EXTEND, DL, MVT::i32, Opr2);
    if (isa<ConstantSDNode>(LoadInp1.getNode()))
      LoadInp1 = LoadOperand(DAG, DL, MVT::i32, LoadInp1);
    if (isa<ConstantSDNode>(LoadInp2.getNode()))
      LoadInp2 = LoadOperand(DAG, DL, MVT::i32, LoadInp2);
    BinOp = DAG.getNode(LowerBinOp, DL, MVT::i32, {LoadInp1, LoadInp2});
  } else {
    LoadInp1 = LoadOperand(DAG, DL, Opr1.getValueType(), Opr1);
    LoadInp2 = LoadOperand(DAG, DL, Opr2.getValueType(), Opr2);
    BinOp = DAG.getNode(LowerBinOp, DL, VT, {LoadInp1, LoadInp2});
  }

  TransferNodeFlagsWithGlue(Op, BinOp);

  if (VT == MVT::i16 || VT == MVT::i8)
    BinOp = DAG.getNode(ISD::TRUNCATE, DL, VT, BinOp);

  return BinOp;
}

static SDValue LowerBR_CC(SDValue Op, SelectionDAG &DAG) {
  SDLoc DL(Op);
  auto GetNewCCode = [=](ISD::CondCode CCode) -> ISD::CondCode {
    switch (CCode) {
    default:
      llvm_unreachable("Unhandled condition in LowerBR_CC");
    case ISD::SETLT:
    case ISD::SETULT:
      return ISD::SETLT;
    case ISD::SETLE:
    case ISD::SETULE:
      return ISD::SETLE;
    case ISD::SETGT:
    case ISD::SETUGT:
      return ISD::SETGT;
    case ISD::SETGE:
    case ISD::SETUGE:
      return ISD::SETGE;
    case ISD::SETEQ:
    case ISD::SETUEQ:
      return ISD::SETEQ;
    case ISD::SETNE:
    case ISD::SETUNE:
      return ISD::SETNE;
    }
  };

  SDValue Chain = Op.getOperand(0);
  SDValue CondNode = Op.getOperand(1);
  SDValue Opr1 = Op.getOperand(2);
  SDValue Opr2 = Op.getOperand(3);
  SDValue BBlock = Op.getOperand(4);
  EVT VT = Opr1.getValueType();

  SDValue LoadInp1 = LoadOperand(DAG, DL, VT, Opr1);
  SDValue LoadInp2 = LoadOperand(DAG, DL, VT, Opr2);

  ISD::CondCode CCode = cast<CondCodeSDNode>(CondNode.getNode())->get();
  if (VT == MVT::i64) {
    SDValue Ops[] = {LoadInp2, LoadInp1};
    SDValue LCmp = DAG.getNode(JVMISD::CMP, DL, MVT::i32, Ops);
    LoadInp1 = LCmp;
    LoadInp2 = LoadOperand(DAG, DL, MVT::i32, DAG.getConstant(0, DL, MVT::i32));
    CCode = GetNewCCode(CCode);
  }
  CondNode = DAG.getConstant(CCode, DL, MVT::i32);
  SDValue Ops[] = {Chain, CondNode, LoadInp1, LoadInp2, BBlock};

  SDValue Brcc = DAG.getNode(JVMISD::BR_CC, DL, MVT::Other, Ops);

  TransferNodeFlagsWithGlue(Op, Brcc);
  return Brcc;
}

static SDValue LowerConstantCondition(SelectionDAG &DAG, SDValue Chain,
                                      SDValue CondNode, SDValue BBlock) {
  SDLoc DL(CondNode);
  assert(CondNode.getOpcode() == ISD::Constant &&
         "Non-constant condition value");

  SDValue LoadInp = LoadOperand(DAG, DL, MVT::i32, CondNode);
  SDValue CondCode = DAG.getConstant(ISD::SETNE, DL, MVT::i32);

  SDValue Ops[] = {Chain, CondCode, LoadInp, BBlock};
  SDValue BrConstcc = DAG.getNode(JVMISD::BR_CONST_CC, DL, MVT::Other, Ops);

  TransferNodeFlagsWithGlue(CondNode, BrConstcc);
  return BrConstcc;
}

static SDValue LowerBRCOND(SDValue Op, SelectionDAG &DAG) {
  SDLoc DL(Op);

  SDValue Chain = Op.getOperand(0);
  SDValue CondNode = Op.getOperand(1);
  SDValue BBlock = Op.getOperand(2);

  // This generally occurs when operands of compare instructions
  // were constants and DAG builder evaluated and folded it to a
  // constant.
  if (CondNode.getOpcode() == ISD::Constant)
    return LowerConstantCondition(DAG, Chain, CondNode, BBlock);

  bool isInv;
  SDValue SetCC;
  if (CondNode.getOpcode() == ISD::XOR &&
      isa<ConstantSDNode>(CondNode.getOperand(1)) &&
      cast<ConstantSDNode>(CondNode.getOperand(1).getNode())->isOne() &&
      CondNode.getOpcode() == ISD::SETCC) {
    isInv = true;
    SetCC = CondNode.getOperand(0);
  } else if (CondNode.getOpcode() == ISD::SETCC) {
    isInv = false;
    SetCC = CondNode;
  } else
    llvm_unreachable("Unhandled conditional code!");

  ISD::CondCode CCode = cast<CondCodeSDNode>(CondNode.getNode())->get();
  if (isInv)
    CCode = ISD::getSetCCInverse(CCode, 1);
  CondNode = DAG.getConstant(CCode, DL, MVT::i32);

  EVT VT = SetCC.getValueType();
  SDValue LoadInp1 = LoadOperand(DAG, DL, VT, SetCC.getOperand(1));
  SDValue LoadInp2 = LoadOperand(DAG, DL, VT, SetCC.getOperand(2));

  // TODO: If one of the load input is constant 0 then generate
  // an appropriate DAG with opcode JVMISD::BR_CONST_CC.
  // Canonicalize condition DAG node and adjust condition code
  // accordingly beforehand.
  SDValue Ops[] = {Chain, CondNode, LoadInp1, LoadInp2, BBlock};

  SDValue Brcc = DAG.getNode(JVMISD::BR_CC, DL, MVT::Other, Ops);

  TransferNodeFlagsWithGlue(Op, Brcc);
  return Brcc;
}

static bool IsCompositeRef(unsigned Opcode) {
  return Opcode == ISD::STRUCT_REF || Opcode == ISD::ARRAY_REF ||
         Opcode == JVMISD::FIELD_ACC_W || Opcode == JVMISD::FIELD_ACC_R ||
         Opcode == JVMISD::INDEX_ACC;
}

/// Memory operations must be in-order, to enforce this
/// both loads and stores must be tied to chain which
/// constrains scheduler from re-ordering them.
static SDValue LowerLOAD(SDValue Op, SelectionDAG &DAG) {
  SDLoc DL(Op);
  SDValue Ptr = Op.getOperand(1);
  if (IsCompositeRef(Ptr.getOpcode())) {
    // Load of composite reference must be an absorption candidate.
    assert(Ptr.getNode()->getFlags().hasIsAbsorptionCandidate());
    return Op;
  }

  assert(Op.getOperand(2).isUndef() && "Load with offset");
  EVT VTs[] = {Op.getValueType(), MVT::Other};
  SDValue Ops[] = {Op.getOperand(0), Ptr};
  return DAG.getNode(JVMISD::LOAD_W_CHAIN, DL, VTs, Ops);
}

static SDValue LowerSTORE(SDValue Op, SelectionDAG &DAG) {
  SDLoc DL(Op);
  SDValue Chain = Op.getOperand(0);
  SDValue Val = Op.getOperand(1);
  SDValue Ptr = Op.getOperand(2);
  if (IsCompositeRef(Ptr.getOpcode())) {
    // Store to a composite reference must be an absorption candidate.
    assert(Ptr.getNode()->getFlags().hasIsAbsorptionCandidate());
    return Op;
  }

  // TODO: LOAD/STORE with offsets to be converted to GEP + LOAD
  // during IR decoration.
  assert(Op.getOperand(3).isUndef() && "Store with offset");
  SDValue LoadVal = LoadOperand(DAG, DL, Val.getValueType(), Val);
  SDValue Ops[] = {Chain, LoadVal, Ptr};
  return DAG.getNode(JVMISD::STORENR, DL, MVT::Other, Ops);
}

static SDValue GetFieldSpecDescID(SDLoc &DL, SelectionDAG &DAG, SDValue &Obj,
                                  SDValue &Idx) {
  JVMFunctionInfo *MFI = DAG.getMachineFunction().getInfo<JVMFunctionInfo>();
  Type *ObjTy = MFI->getFromNodeTypeMap(Obj.getNode());

  assert(ObjTy && "Object Type not found in NodeTypeMap");
  assert(isa<StructType>(ObjTy) && "Cannot fetch desc for non struct object");
  assert(isa<ConstantSDNode>(Idx.getNode()));

  uint64_t IdxNum = cast<ConstantSDNode>(Idx.getNode())->getZExtValue();

  Type *FTy = cast<StructType>(ObjTy)->getElementType((unsigned)IdxNum);
  char fbuff[128] = {'\0'};
  sprintf(fbuff, "%ld", IdxNum);

  std::string FieldSpec = MFI->getFromTypeNameMap(ObjTy);
  FieldSpec += "/f";
  FieldSpec += fbuff;
  FieldSpec += " ";
  MFI->getTypeDescriptor(FTy, FieldSpec);

  unsigned DescID = MFI->addToDescriptorIDMap(FieldSpec);
  return DAG.getConstant(DescID, DL, MVT::i32);
}

static int GetChainOperandNumFromValue(SDValue Val) {
  SDNode *Node = Val.getNode();
  unsigned N = Node->getNumValues();
  while (N && Node->getValueType(N - 1) == MVT::Glue)
    --N;
  if (N && Node->getValueType(N - 1) == MVT::Other)
    return N - 1;
  return -1;
}

static void ReplaceAllUsesOfValueAndChain(bool RepOnlyChain, SelectionDAG &DAG,
                                          SDValue Old, SDValue New) {
  int OldChNum, NewChNum;
  if (false == RepOnlyChain)
    DAG.ReplaceAllUsesOfValueWith(Old, New);

  OldChNum = GetChainOperandNumFromValue(Old);
  NewChNum = GetChainOperandNumFromValue(New);
  // Both nodes have chain replace it.
  if (OldChNum >= 0 && NewChNum >= 0)
    DAG.ReplaceAllUsesOfValueWith(Old.getValue(OldChNum),
                                  New.getValue(NewChNum));
  else if (OldChNum >= 0) {
    // Old Value's first operand is it parent chain,
    // replace its chain uses with its parents chain.
    assert(Old.getOperand(0).getValueType().getSimpleVT() == MVT::Other);
    SDValue OldParentCh = Old.getOperand(0);
    int OldParentChNum = GetChainOperandNumFromValue(OldParentCh);
    assert(OldParentCh.getNode()->getValueType(OldParentChNum).getSimpleVT() ==
           MVT::Other);
    DAG.ReplaceAllUsesOfValueWith(Old.getValue(OldChNum),
                                  OldParentCh.getValue(OldParentChNum));
  }
}

static SDValue GetRefUse(SDValue CompRef) {
  SDNode *User = nullptr;
  SDNode *CompRefNode = CompRef.getNode();
  for (SDNode::use_iterator itr = CompRefNode->use_begin();
       itr != CompRefNode->use_end(); itr++) {
    SDUse &Use = itr.getUse();
    if (CompRefNode != Use.getUser() && Use.getValueType() != MVT::Other) {
      User = *itr;
      break;
    }
  }
  return SDValue(User, 0);
}

#if 0
static SDValue GetSOCompRefChain(SelectionDAG &DAG, SDLoc &DL, SDValue Chain,
                                 SDValue Val) {
  SDNodeFlags Flags = Val.getNode()->getFlags();
  if (!Flags.hasIsArrayAcc() && !Flags.hasIsStructAcc()) {
    return SDValue();
  }

  Chain = Val.getValue(1);
  SDNodeFlags ChainFlags = Chain.getNode()->getFlags();
  while (!ChainFlags.hasIsComposite()) {
    Chain = Val.getValue(1);
    ChainFlags = Chain.getNode()->getFlags();
  }
  return Chain;
}
#endif

// STRUCT_REF <CHAIN> <OBJ> <FIELD INDEX> <IN/OUT CHAIN>
static SDValue LowerSTRUCT_REF(SDValue Op, SelectionDAG &DAG) {
  SDLoc DL(Op);
  SDValue Chain = Op.getOperand(0);
  SDValue Obj = Op.getOperand(1);
  SDValue Idx = Op.getOperand(2);
  SDValue InChainUse = Op.getOperand(3);
  int InChainUseFlag =
      cast<ConstantSDNode>(InChainUse.getNode())->getZExtValue();

  // GEP chain cloning done at IR level makes sure that each
  // Load/Store get its complete GEP chain before it which counters
  // sharing of GEPs due to CSE.

  SDValue Use = GetRefUse(Op);

  SDValue Aload = Obj;
  if (IS_START_OF_CHAIN(InChainUseFlag)) {
    EVT VTs[] = {MVT::i32, MVT::Other};
    SDValue Ops[] = {Chain, Obj};
    Aload = DAG.getNode(JVMISD::LOAD_W_CHAIN, DL, VTs, Ops);
    // Setting IsCompositeFlag over Load of an object.
    SDNodeFlags Flags = Aload.getNode()->getFlags();
    Flags.setIsComposite(true);
    Aload.getNode()->setFlags(Flags);
    Chain = Aload.getValue(1);
  }

  SDValue DescID = GetFieldSpecDescID(DL, DAG, Obj, Idx);
  SDNodeFlags Flags = Op.getNode()->getFlags();
  Idx.getNode()->setFlags(Flags);

  SDValue FieldAcc;
  if (Use.getOpcode() == ISD::STORE) {
    SDNode *AloadNode = Aload.getNode();
    assert(AloadNode->getValueType(AloadNode->getNumValues() - 1) ==
               MVT::Other &&
           "Aload not connected to chain");
    EVT VTs[] = {Use.getOperand(1).getValueType(), MVT::Other};
    SDValue StoreSrc[] = {Chain, Use.getOperand(1)};
    SDValue SrcLoad = LoadOperandWCh(DAG, DL, VTs, StoreSrc);

    TransferNodeFlags(Use.getOperand(1), SrcLoad);

    // PUTFIELD = FIELD_ACC_W <CHAIN> <LOADED_OBJ> <SRC> <DESC-ID>
    SDValue Opr[] = {SrcLoad.getValue(1), Aload, SrcLoad, DescID};
    FieldAcc = DAG.getNode(JVMISD::FIELD_ACC_W, DL, MVT::Other, Opr);

    // Transfer IsStructAcc with [IsComposite] to new FieldAcc.
    TransferNodeFlags(Op, FieldAcc);
  } else {
    // GETFIELD = FIELD_ACC_R <CHAIN> <LOADED_OBJ> <DESC-ID>
    EVT VTs[] = {Op.getValueType(), MVT::Other};
    SDValue Opr[] = {Chain, Aload, DescID};
    FieldAcc = DAG.getNode(JVMISD::FIELD_ACC_R, DL, VTs, Opr);
    Chain = FieldAcc.getValue(1);

    // Transfer IsStructAcc with [IsComposite] to new FieldAcc.
    TransferNodeFlags(Op, FieldAcc);

    // Break any further dependency over this composite load
    // by spilling the loaded field over stack.
    // TODO: This will be optimized during a separate
    // redundant LOAD/STORE elimination pass during MI phase.
    if (Use.getOpcode() == ISD::LOAD) {
      EVT StoreVTs[] = {FieldAcc.getValueType(), MVT::Other};
      SDValue StoreOps[] = {Chain, FieldAcc};
      FieldAcc = DAG.getNode(JVMISD::STORE, DL, StoreVTs, StoreOps);
      Chain = FieldAcc.getValue(1);

      SDNodeFlags Flags;
      Flags.setIsComposite(Op.getNode()->getFlags().hasIsComposite());
      FieldAcc.getNode()->setFlags(Flags);
    }
  }

  if (Use.getOpcode() == ISD::LOAD || Use.getOpcode() == ISD::STORE)
    ReplaceAllUsesOfValueAndChain(false, DAG, Use, FieldAcc);

  ReplaceAllUsesOfValueAndChain(false, DAG, Op, FieldAcc);
  Flags = FieldAcc.getNode()->getFlags();
  Flags.setIsUseAbsorb(true);
  FieldAcc.getNode()->setFlags(Flags);
  return FieldAcc;
}

// ARRAY_REF <CHAIN> <OBJ> <INDEX> <IN/OUT CHAIN>
static SDValue LowerARRAY_REF(SDValue Op, SelectionDAG &DAG) {
  SDLoc DL(Op);
  SDValue Chain = Op.getOperand(0);
  SDValue Obj = Op.getOperand(1);
  SDValue Idx = Op.getOperand(2);
  SDValue InChainUse = Op.getOperand(3);
  int InChainUseFlag =
      cast<ConstantSDNode>(InChainUse.getNode())->getZExtValue();

  SDValue Use = GetRefUse(Op);

  SDValue Aload = Obj;
  if (IS_START_OF_CHAIN(InChainUseFlag)) {
    EVT VTs[] = {MVT::i32, MVT::Other};
    SDValue Ops[] = {Chain, Obj};
    Aload = DAG.getNode(JVMISD::LOAD_W_CHAIN, DL, VTs, Ops);
    // Setting IsCompositeFlag over Load of an object.
    SDNodeFlags Flags = Aload.getNode()->getFlags();
    Flags.setIsComposite(true);
    Aload.getNode()->setFlags(Flags);
    Chain = Aload.getValue(1);
  }

  EVT VTs[] = {MVT::i32, MVT::Other};
  SDValue Ops[] = {Chain, Aload, Idx};
  SDValue IdxNode = DAG.getNode(JVMISD::INDEX_ACC, DL, VTs, Ops);
  Chain = IdxNode.getValue(1);

  SDValue ArrRef;
  SDValue NewChain;
  if (Use.getOpcode() == ISD::STORE) {
    EVT VTs[] = {Use.getOperand(1).getValueType(), MVT::Other};
    SDValue Src[] = {Chain, Use.getOperand(1)};
    SDValue SrcLoad = LoadOperandWCh(DAG, DL, VTs, Src);

    TransferNodeFlags(Use.getOperand(1), SrcLoad);

    SDValue Ops[] = {SrcLoad.getValue(1), SrcLoad, IdxNode};
    ArrRef = DAG.getNode(JVMISD::STORENR, DL, MVT::Other, Ops);

    // Transfer IsArrayAcc and [IsComposite] over newarray ref node.
    TransferNodeFlags(Op, ArrRef);

    NewChain = IdxNode.getValue(1);
  } else {
    EVT VTs[] = {Op.getValueType(), MVT::Other};
    SDValue Ops[] = {Chain, IdxNode};
    ArrRef = DAG.getNode(JVMISD::LOAD_W_CHAIN, DL, VTs, Ops);
    NewChain = ArrRef.getValue(1);

    // Transfer IsArrayAcc and [IsComposite] over newarray ref node.
    TransferNodeFlags(Op, ArrRef);

    if (Use.getOpcode() == ISD::LOAD) {
      EVT StoreVTs[] = {ArrRef.getValueType(), MVT::Other};
      SDValue StoreOps[] = {NewChain, ArrRef};
      ArrRef = DAG.getNode(JVMISD::STORE, DL, StoreVTs, StoreOps);
      NewChain = ArrRef.getValue(1);

      SDNodeFlags Flags;
      Flags.setIsComposite(Op.getNode()->getFlags().hasIsComposite());
      ArrRef.getNode()->setFlags(Flags);
    }
    DAG.ReplaceAllUsesOfValueWith(Op, ArrRef);
  }

  SDNodeFlags Flags = ArrRef.getNode()->getFlags();
  Flags.setIsUseAbsorb(true);
  ArrRef.getNode()->setFlags(Flags);

  if (Use.getOpcode() == ISD::LOAD || Use.getOpcode() == ISD::STORE)
    ReplaceAllUsesOfValueAndChain(false, DAG, Use, ArrRef);

  DAG.ReplaceAllUsesOfValueWith(Op.getValue(1), NewChain);
  return ArrRef;
}

static std::string GetTypeString(SelectionDAG &DAG, Type *CTy) {
  JVMFunctionInfo *MFI = DAG.getMachineFunction().getInfo<JVMFunctionInfo>();
  if (isa<StructType>(CTy))
    return MFI->getFromTypeNameMap(CTy);
  else if (isa<ArrayType>(CTy))
    return GetTypeString(DAG, cast<ArrayType>(CTy)->getElementType());

  MVT VT = MVT::getVT(CTy);
  switch (VT.SimpleTy) {
  default:
    break;
  case MVT::i1:
    return "bool";
  case MVT::i8:
    return "byte";
  case MVT::i16:
    return "short";
  case MVT::i32:
    return "int";
  case MVT::i64:
    return "long";
  }
  llvm_unreachable("Unsupported type string requested");
  return "";
}

static SDValue GenStoreForAllocatedMem(SDValue Mem, SDLoc &dl,
                                       SelectionDAG &DAG, bool isComposite) {
  assert(Mem.getNode()->getValueType(Mem.getNode()->getNumValues() - 1) ==
         MVT::Other);
  SDValue Chain = Mem.getValue(1);
  EVT AllocStoreVTs[] = {Mem.getValueType(), MVT::Other};
  SDValue AllocStoreOps[] = {Chain, Mem};
  SDValue AllocStore =
      DAG.getNode(JVMISD::STORE, dl, AllocStoreVTs, AllocStoreOps);
  SDNodeFlags Flags = AllocStore.getNode()->getFlags();
  Flags.setIsComposite(isComposite);
  AllocStore.getNode()->setFlags(Flags);
  return AllocStore;
}

static SDValue GenConstructorCallSequence(Type *AllocatedTy, SDValue Alloc,
                                          SDLoc &dl, SelectionDAG &DAG) {
  JVMFunctionInfo *MFI = DAG.getMachineFunction().getInfo<JVMFunctionInfo>();
  SDValue Chain = Alloc.getValue(1);
  EVT DupVTs[] = {Alloc.getValueType(), MVT::Other};
  SDValue DupOps[] = {Chain, Alloc};
  SDValue Dup = DAG.getNode(JVMISD::DUP, dl, DupVTs, DupOps);
  Chain = Dup.getValue(1);

  std::string MethodDescStr = MFI->getFromTypeNameMap(AllocatedTy);
  MethodDescStr += "/<init>()V";
  unsigned DescID = MFI->addToDescriptorIDMap(MethodDescStr);
  SDValue MethodDesc = DAG.getConstant(DescID, dl, MVT::i32);

  EVT InvokeVTs[] = {Dup.getValueType(), MVT::Other};
  SDValue InvokeOps[] = {Chain, Dup, MethodDesc};
  SDValue InvokeNv = DAG.getNode(JVMISD::INVOKENV, dl, InvokeVTs, InvokeOps);
  Chain = InvokeNv.getValue(1);

  return InvokeNv;
}

static SDValue LowerDYNAMIC_STACKALLOC(SDValue Op, SelectionDAG &DAG) {
  SDLoc dl(Op);
  SDValue Alloc;
  SDValue Chain = Op.getOperand(0);

  // ALLOCA Chain <LoadSize> <TypeName-DescID> <Constant IsArray>
  // <Constant IsClass>
  JVMFunctionInfo *MFI = DAG.getMachineFunction().getInfo<JVMFunctionInfo>();
  Type *AllocatedTy = MFI->getFromNodeTypeMap(Op.getNode());

  SDValue LoadSz = DAG.getConstant(1, dl, MVT::i32);

  std::string TypeStr = GetTypeString(DAG, AllocatedTy);
  unsigned DescID = MFI->addToDescriptorIDMap(TypeStr);
  SDValue TypeStrID = DAG.getConstant(DescID, dl, MVT::i32);

  SDValue ArrSz = Op.getOperand(1);
  bool IsVarOrNonUnitSize =
      (!isa<ConstantSDNode>(ArrSz.getNode()) ||
       cast<ConstantSDNode>(ArrSz.getNode())->getZExtValue() != 1);
  if (IsVarOrNonUnitSize)
    LoadSz = LoadOperand(DAG, dl, ArrSz.getValueType(), ArrSz);

  // Generate appropriate allocation nodes.
  EVT VTs[] = {Op.getValueType(), MVT::Other};
  if (isa<ArrayType>(AllocatedTy)) {
    int IsClassArr =
        isa<StructType>(cast<ArrayType>(AllocatedTy)->getElementType());
    int ArrSz = cast<ArrayType>(AllocatedTy)->getNumElements();
    SDValue ArrSzVal = DAG.getConstant(ArrSz, dl, MVT::i32);
    EVT LoadVTs[] = {ArrSzVal.getValueType(), MVT::Other};
    SDValue LoadOps[] = {Chain, ArrSzVal};
    LoadSz = DAG.getNode(JVMISD::LOAD_W_CHAIN, dl, LoadVTs, LoadOps);
    Chain = LoadSz.getValue(1);

    SDValue Ops[] = {Chain, LoadSz, TypeStrID, DAG.getConstant(1, dl, MVT::i32),
                     DAG.getConstant(IsClassArr, dl, MVT::i32)};
    Alloc = DAG.getNode(JVMISD::ALLOC, dl, VTs, Ops);
  } else if (isa<StructType>(AllocatedTy)) {
    SDValue Ops[] = {Chain, LoadSz, TypeStrID, DAG.getConstant(0, dl, MVT::i32),
                     DAG.getConstant(1, dl, MVT::i32)};
    Alloc = DAG.getNode(JVMISD::ALLOC, dl, VTs, Ops);
  } else {
    int IsArray = IsVarOrNonUnitSize ? 1 : 0;
    SDValue Ops[] = {Chain, LoadSz, TypeStrID,
                     DAG.getConstant(IsArray, dl, MVT::i32),
                     DAG.getConstant(0, dl, MVT::i32)};
    Alloc = DAG.getNode(JVMISD::ALLOC, dl, VTs, Ops);
  }

  bool IsPrimAlloc = !cast<ConstantSDNode>(Alloc.getOperand(3))->isOne() &&
                     !cast<ConstantSDNode>(Alloc.getOperand(4))->isOne();

  if (isa<StructType>(AllocatedTy))
    Alloc = GenConstructorCallSequence(AllocatedTy, Alloc, dl, DAG);

  // Store the allocated memory to a local variable.
  if (!IsPrimAlloc)
    return GenStoreForAllocatedMem(Alloc, dl, DAG,
                                   isa<CompositeType>(AllocatedTy));
  return Alloc;
}

static SDValue LowerEXTEND(SDValue Op, SelectionDAG &DAG) {
  SDLoc DL(Op);

  SDValue Op0 = Op.getOperand(0);
  if (Op.getOpcode() == ISD::SIGN_EXTEND_INREG)
    return Op0;

  Op0 = LoadOperand(DAG, DL, Op0.getValueType(), Op0);
  SDValue Extend = DAG.getNode(JVMISD::EXTEND, DL, Op.getValueType(), Op0);
  return Extend;
}

static SDValue LowerTRUNCATE(SDValue Op, SelectionDAG &DAG) {
  SDLoc DL(Op);
  SDValue Op0 = Op.getOperand(0);
  EVT Op0VT = Op0.getValueType();
  EVT TruncVT = Op.getValueType();

  // Fundamental object which can go over JVM frame's Local variable
  // array is 32 bit integral value.
  if (TruncVT.bitsLT(MVT::i32) && Op0VT.bitsGT(MVT::i32)) {
    SDValue Load = DAG.getNode(JVMISD::LOAD, DL, Op0VT, Op0);
    Op0 = DAG.getNode(JVMISD::TRUNC, DL, MVT::i32, Load);
    Op0VT = Op0.getValueType();
  }

  SDValue Load = DAG.getNode(JVMISD::LOAD, DL, Op0VT, Op0);
  SDValue Trunc = DAG.getNode(JVMISD::TRUNC, DL, TruncVT, Load);
  return Trunc;
}

static SDValue LowerGlobalAddress(SDValue Op, SelectionDAG &DAG, EVT PtrVT) {
  SDLoc DL(Op);
  const GlobalValue *GV = cast<GlobalAddressSDNode>(Op)->getGlobal();
  return DAG.getTargetGlobalAddress(GV, DL, PtrVT);
}

SDValue JVMTargetLowering::LowerOperation(SDValue Op, SelectionDAG &DAG) const {
  SDLoc DL(Op);
  auto PtrVT = getPointerTy(DAG.getDataLayout());
  switch (Op.getOpcode()) {
  default:
    llvm_unreachable("unimplemented operation lowering");
    return SDValue();
  case ISD::ARRAY_REF:
    return LowerARRAY_REF(Op, DAG);
  case ISD::STRUCT_REF:
    return LowerSTRUCT_REF(Op, DAG);
  case ISD::BRCOND:
    return LowerBRCOND(Op, DAG);
  case ISD::BR_CC:
    return LowerBR_CC(Op, DAG);
  case ISD::DYNAMIC_STACKALLOC:
    return LowerDYNAMIC_STACKALLOC(Op, DAG);
  case ISD::SHL:
  case ISD::SRA:
  case ISD::SRL:
  case ISD::OR:
  case ISD::AND:
  case ISD::XOR:
  case ISD::ADD:
  case ISD::SUB:
  case ISD::MUL:
  case ISD::UDIV:
  case ISD::SDIV:
  case ISD::UREM:
  case ISD::SREM:
    return LowerBinaryOp(Op, DAG);
  case ISD::LOAD:
    return LowerLOAD(Op, DAG);
  case ISD::STORE:
    return LowerSTORE(Op, DAG);
  case ISD::GlobalAddress:
    return LowerGlobalAddress(Op, DAG, PtrVT);
  case ISD::ZERO_EXTEND:
  case ISD::SIGN_EXTEND:
  case ISD::ANY_EXTEND:
  case ISD::SIGN_EXTEND_INREG:
    return LowerEXTEND(Op, DAG);
  case ISD::TRUNCATE:
    return LowerTRUNCATE(Op, DAG);
  }
}

void JVMTargetLowering::clearAuxiliaryData() {
  RepMap.clear();
  Replicator = 0;
  // TODO: MachineInfo maps clearing.
}

//===----------------------------------------------------------------------===//
//                          JVM Optimization Hooks
//===----------------------------------------------------------------------===//

SDValue JVMTargetLowering::PerformDAGCombine(SDNode *N,
                                             DAGCombinerInfo &DCI) const {
  // TODO : Perform custom DAG combining operations.
  return SDValue(N, 0);
}
