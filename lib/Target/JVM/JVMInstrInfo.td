// JVMInstrInfo.td-Describe the JVM Instructions-*- tablegen -*-
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
///
/// \file
/// \brief JVM Instruction definitions.
///
//===----------------------------------------------------------------------===//

//===----------------------------------------------------------------------===//
// JVM Instruction Predicate Definitions.
//===----------------------------------------------------------------------===//

def HasAddr32 : Predicate<"!Subtarget->hasAddr64()">;
def HasAddr64 : Predicate<"Subtarget->hasAddr64()">;

def SDT_JVMCall     : SDTypeProfile<0, -1, [SDTCisPtrTy<0>]>;
def SDT_JVMArgument : SDTypeProfile<1, 1, [SDTCisVT<1, i32>]>;
def SDT_JVMReturn   : SDTypeProfile<0, -1, []>;
def SDT_JVMWrapper  : SDTypeProfile<1, 1, [SDTCisSameAs<0, 1>,
                                                 SDTCisPtrTy<0>]>;
def SDT_JVMILoad  : SDTypeProfile<1, 1, [     // i32/i64 load
  SDTCisInt<1> , SDTCisSameAs<0,1>
]>;
def SDT_JVMFLoad  : SDTypeProfile<1, 1, [     // f32/f64 load
  SDTCisFP<1> , SDTCisSameAs<0,1>
]>;

def SDT_JVMIStoreNoRes : SDTypeProfile<0, 2, [ // i32/i64 store
  SDTCisInt<0> , SDTCisInt<1>
]>;

def SDT_JVMFStoreNoRes : SDTypeProfile<0, 2, [ // f32/f64 store
  SDTCisFP<0> , SDTCisFP<1>
]>;

def SDT_JVMIStore : SDTypeProfile<1, 1,      [ // i32/i64 store
  SDTCisInt<0>, SDTCisInt<1>
]>;   

def SDT_JVMBrcc : SDTypeProfile<0, 4, [       // jvmbrcc
  SDTCisInt<0>, SDTCisSameAs<1, 2>, SDTCisVT<3, OtherVT>
]>;

def SDT_JVMCmp : SDTypeProfile<1, 2, [        // jvmcmp
  SDTCisInt<0>, SDTCisSameAs<1, 2>
]>;

def SDT_JVMBrConstcc : SDTypeProfile<0, 3, [  // jvmbrconstcc
  SDTCisInt<0>, SDTCisInt<1>, SDTCisVT<2, OtherVT>
]>;

def SDT_JVMIdxAcc : SDTypeProfile<1, 2 , [    // jvmidxacc
  SDTCisInt<0>, SDTCisInt<1>, SDTCisInt<2>
]>; 

def SDT_JVMFldAccW : SDTypeProfile<0, 3 , [   // jvmfldaccw
  SDTCisInt<0> , SDTCisInt<1>, SDTCisInt<2>
]>;

def SDT_JVMFldAccR : SDTypeProfile<1, 2 , [   // jvmfldaccr
  SDTCisInt<0> ,SDTCisInt<1> , SDTCisInt<2>
]>;

def SDT_JVMNewAlloc : SDTypeProfile<1, 4, [   // jvmnewalloc
  SDTCisInt<0>, SDTCisSameAs<1,2> , SDTCisInt<2>,
  SDTCisSameAs<3,4> , SDTCisInt<3>
]>;

def SDT_JVMInvokeNV : SDTypeProfile<1, 2, [   // jvminvokenv
   SDTCisSameAs<0,1> , SDTCisInt<1>, SDTCisInt<2>
]>;

def SDT_JVMDup : SDTypeProfile<1, 1,      [   // jvmdup
   SDTCisSameAs<0,1>, SDTCisInt<1>
]>;

def SDT_JVMTrunc : SDTypeProfile<1, 1,      [   // jvmext
   SDTCisInt<0>, SDTCisInt<1>
]>;

def SDT_JVMExt : SDTypeProfile<1, 1,      [   // jvmext
   SDTCisInt<0>, SDTCisInt<1>
]>;

def imm8bit         : IntImmLeaf<i8 , [{}]>;
def imm16bit        : IntImmLeaf<i16 , [{}]>;
def imm32bit        : IntImmLeaf<i32 , [{}]>;
def imm64bit        : IntImmLeaf<i64 , [{}]>;
def imm0            : IntImmLeaf<i32 , [{ return Imm == 0; }]>;
def imm1            : IntImmLeaf<i32 , [{ return Imm == 1; }]>;
def imm2            : IntImmLeaf<i32 , [{ return Imm == 2; }]>;
def imm3            : IntImmLeaf<i32 , [{ return Imm == 3; }]>;
def imm4            : IntImmLeaf<i32 , [{ return Imm == 4; }]>;
def imm5            : IntImmLeaf<i32 , [{ return Imm == 5; }]>;

def limm0           : IntImmLeaf<i64 , [{ return Imm == 0; }]>;
def limm1           : IntImmLeaf<i64 , [{ return Imm == 1; }]>;


def fimm0           : FPImmLeaf<f32 , 
                         [{ return Imm.compare(APFloat(0.0)) == APFloat::cmpEqual; }]>;
def fimm1           : FPImmLeaf<f32 , 
                         [{ return Imm.compare(APFloat(1.0)) == APFloat::cmpEqual; }]>;

def dimm0           : FPImmLeaf<f64 , 
                         [{ return Imm.compare(APFloat(0.0)) == APFloat::cmpEqual; }]>;
def dimm1           : FPImmLeaf<f64 , 
                         [{ return Imm.compare(APFloat(1.0)) == APFloat::cmpEqual; }]>;

//===----------------------------------------------------------------------===//
// JVM-specific DAG Nodes.
//===----------------------------------------------------------------------===//

def JVMcall     : SDNode<"JVMISD::CALL",
                              SDT_JVMCall,
                              [SDNPHasChain, SDNPVariadic]>;
def JVMargument : SDNode<"JVMISD::ARGUMENT",
                                 SDT_JVMArgument>;
def JVMreturn   : SDNode<"JVMISD::RET",
                                 SDT_JVMReturn, [SDNPHasChain]>;
def JVMwrapper  : SDNode<"JVMISD::Wrapper",
                                 SDT_JVMWrapper>;
def JVMbrcc     : SDNode<"JVMISD::BR_CC", SDT_JVMBrcc,
                         [SDNPHasChain]>;
def JVMCmp      : SDNode<"JVMISD::CMP", SDT_JVMCmp,
                         []>;
def JVMbrconstcc: SDNode<"JVMISD::BR_CONST_CC", SDT_JVMBrConstcc,
                         [SDNPHasChain]>;

// TODO: Remove following binary DAG nodes once load insertion is
// done down the pipeline.
def iadd        : SDNode<"JVMISD::ADD"       , SDTIntBinOp,
                         [SDNPCommutative, SDNPAssociative]>;
def isub        : SDNode<"JVMISD::SUB"       , SDTIntBinOp>;
def imul        : SDNode<"JVMISD::MUL"       , SDTIntBinOp,
                         [SDNPCommutative, SDNPAssociative]>;
def idiv        : SDNode<"JVMISD::DIV"       , SDTIntBinOp,
                         []>;
def irem        : SDNode<"JVMISD::REM"       , SDTIntBinOp,
                         []>;

def ior         : SDNode<"JVMISD::OR"        , SDTIntBinOp,
                         [SDNPCommutative, SDNPAssociative]>;
def ixor        : SDNode<"JVMISD::XOR"       , SDTIntBinOp,
                         [SDNPCommutative, SDNPAssociative]>;
def iand        : SDNode<"JVMISD::AND"       , SDTIntBinOp,
                         [SDNPCommutative, SDNPAssociative]>;

def ishl        : SDNode<"JVMISD::SHL"       , SDTIntShiftOp,
                         []>;
def isrl        : SDNode<"JVMISD::SRL"       , SDTIntShiftOp,
                         []>;
def isra        : SDNode<"JVMISD::SRA"       , SDTIntShiftOp,
                         []>;

// Type specific loads and stores nodes.
def jvmiload    : SDNode<"JVMISD::LOAD"     , SDT_JVMILoad,
                         [SDNPMayLoad]>;
def jvmfload    : SDNode<"JVMISD::LOAD"     , SDT_JVMFLoad,
                         [SDNPMayLoad]>;

def jvmiloadwch : SDNode<"JVMISD::LOAD_W_CHAIN"     , SDT_JVMILoad,
                         [SDNPHasChain ,SDNPMayLoad]>;
def jvmfloadwch : SDNode<"JVMISD::LOAD_W_CHAIN"     , SDT_JVMFLoad,
                         [SDNPHasChain, SDNPMayLoad]>;

def jvmistore   : SDNode<"JVMISD::STORENR"    , SDT_JVMIStoreNoRes,
                         [SDNPHasChain, SDNPMayStore]>;
def jvmfstore   : SDNode<"JVMISD::STORENR"    , SDT_JVMFStoreNoRes,
                         [SDNPHasChain, SDNPMayStore]>;

def jvmistoreres : SDNode<"JVMISD::STORE"   , SDT_JVMIStore, 
                           [SDNPMayStore, SDNPHasChain]>;

def jvmidxacc   : SDNode<"JVMISD::INDEX_ACC", SDT_JVMIdxAcc,
                          [SDNPHasChain]>;

def jvmfldaccw  : SDNode<"JVMISD::FIELD_ACC_W" , SDT_JVMFldAccW,
                          [SDNPHasChain, SDNPMayStore]>;

def jvmfldaccr  : SDNode<"JVMISD::FIELD_ACC_R", SDT_JVMFldAccR,
                          [SDNPHasChain, SDNPMayLoad]>;

def jvmnewalloc : SDNode<"JVMISD::ALLOC", SDT_JVMNewAlloc, 
                          [SDNPHasChain]>;

def jvminvokenv : SDNode<"JVMISD::INVOKENV", SDT_JVMInvokeNV,
                          [SDNPHasChain]>;

def jvmdup      : SDNode<"JVMISD::DUP", SDT_JVMDup,
                          [SDNPHasChain]>;

def jvmext      : SDNode<"JVMISD::EXTEND", SDT_JVMExt,
                          [SDNPMayLoad]>;

def jvmtrunc    : SDNode<"JVMISD::TRUNC", SDT_JVMTrunc, []>;

//===----------------------------------------------------------------------===//
// JVM-specific Operands.
//===----------------------------------------------------------------------===//

let OperandNamespace = "JVM" in {

let OperandType = "OPERAND_BASIC_BLOCK" in
def bb_op : Operand<OtherVT>;

let OperandType = "OPERAND_LOCAL" in
def local_op : Operand<i32>;

let OperandType = "OPERAND_GLOBAL" in
def global_op : Operand<i32>;

let OperandType = "OPERAND_I8IMM" in
def i8imm_op : Operand<i8>;

let OperandType = "OPERAND_I16IMM" in
def i16imm_op : Operand<i16>;

let OperandType = "OPERAND_I32IMM" in
def i32imm_op : Operand<i32>;

let OperandType = "OPERAND_I64IMM" in
def i64imm_op : Operand<i64>;

let OperandType = "OPERAND_F32IMM" in
def f32imm_op : Operand<f32>;

let OperandType = "OPERAND_F64IMM" in
def f64imm_op : Operand<f64>;

let OperandType = "OPERAND_FUNCTION32" in
def function32_op : Operand<i32>;

let OperandType = "OPERAND_OFFSET32" in
def offset32_op : Operand<i32>;

let OperandType = "OPERAND_DESC" in {
def desc_op : Operand<i32> {
  let PrintMethod = "printJVMDescriptorOperand";
}
} 

} // OperandNamespace = "JVM"

//===----------------------------------------------------------------------===//
// JVM Instruction Format Definitions.
//===----------------------------------------------------------------------===//

include "JVMInstrFormats.td"

//===----------------------------------------------------------------------===//
// Additional instructions.
//===----------------------------------------------------------------------===//

multiclass ARGUMENT<JVMRegClass vt> {
  let hasSideEffects = 1, Uses = [ARGUMENTS], isCodeGenOnly = 1 in
  def ARGUMENT : I<(outs vt:$res), (ins i32imm:$argno),
                       [(set vt:$res, (JVMargument timm:$argno))],
                       "" , -1 >;
}

defm I8: ARGUMENT<I8>;
defm I16: ARGUMENT<I16>;
defm I32: ARGUMENT<I32>;
defm I64: ARGUMENT<I64>;
defm F32: ARGUMENT<F32>;
defm F64: ARGUMENT<F64>;

// Following type specific stores are not generated by instruction selection.
let mayStore = 1 , isAsCheapAsAMove = 1 in  {
  def ISTORE : I<(outs), (ins i32imm_op:$index), [],
                        "istore\t$index", 0x36>;
  def FSTORE : I<(outs), (ins i32imm_op:$index), [],
                        "fstore\t$index", 0x38>;
}

//===----------------------------------------------------------------------===//
// Additional sets of instructions.
//===----------------------------------------------------------------------===//

include "JVMInstrInteger.td"
include "JVMInstrControl.td"
include "JVMInstrMemory.td"
include "JVMInstrMisc.td"

