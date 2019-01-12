//===- JVMInstrControl.td-JVM control-flow ------*- tablegen -*-
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
///
/// \file
/// \brief JVM control-flow code-gen constructs.
///
//===----------------------------------------------------------------------===//

// Conditional code predicates
def JVM_CC_EQ  : IntImmLeaf< i32,
                         [{return Imm == ISD::SETEQ || Imm == ISD::SETUEQ;}]>;
def JVM_CC_NE  : IntImmLeaf< i32,
                         [{return Imm == ISD::SETNE || Imm == ISD::SETUNE;}]>;
def JVM_CC_LT  : IntImmLeaf< i32,
                         [{return Imm == ISD::SETLT || Imm == ISD::SETULT;}]>;
def JVM_CC_LE  : IntImmLeaf< i32,
                         [{return Imm == ISD::SETLE || Imm == ISD::SETULE;}]>;
def JVM_CC_GT  : IntImmLeaf< i32,
                         [{return Imm == ISD::SETGT || Imm == ISD::SETUGT;}]>;
def JVM_CC_GE  : IntImmLeaf< i32,
                         [{return Imm == ISD::SETGE || Imm == ISD::SETUGE;}]>;

let isReturn = 1 , isTerminator = 1, hasCtrlDep = 1, isBarrier = 1 in {
def IRETURN8 : I<(outs), (ins I8:$val), [(JVMreturn I8:$val)],
                "ireturn", 0xac>;
def IRETURN16 : I<(outs), (ins I16:$val), [(JVMreturn I16:$val)],
                "ireturn", 0xac>;
def IRETURN32 : I<(outs), (ins I32:$val), [(JVMreturn I32:$val)],
                "ireturn", 0xac>;

def LRETURN : I<(outs), (ins I64:$val), [(JVMreturn I64:$val)],
                "lreturn", 0xad>;

def FRETURN : I<(outs), (ins F32:$val), [(JVMreturn F32:$val)],
                "freturn", 0xae>;

def DRETURN : I<(outs), (ins F64:$val), [(JVMreturn F64:$val)],
                "dreturn", 0xaf>;

def RETURN  : I<(outs), (ins), [(JVMreturn)] , "return", 0xb1>; 
}

let isCompare = 1 in {
def LCMP : I<(outs I32:$res), (ins I64:$arg1, I64:$arg2),
        [(set I32:$res, (JVMCmp I64:$arg1 , I64:$arg2))], "lcmp", 0x94>;
}

let isTerminator = 1 , isBranch = 1 in  {
def IFEQ : I<(outs), (ins i32imm:$cc, I32:$arg1, bb_op:$dst),
       [(JVMbrconstcc JVM_CC_EQ:$cc , I32:$arg1, bb:$dst)] ,"ifeq\t$dst", 0x99>;
def IFNE : I<(outs), (ins i32imm:$cc, I32:$arg1, bb_op:$dst),
       [(JVMbrconstcc JVM_CC_NE:$cc , I32:$arg1, bb:$dst)] ,"ifne\t$dst", 0x9a>;
def IFGT : I<(outs), (ins i32imm:$cc, I32:$arg1, bb_op:$dst),
       [(JVMbrconstcc JVM_CC_GT:$cc , I32:$arg1, bb:$dst)] ,"ifgt\t$dst", 0x9d>;
def IFGE : I<(outs), (ins i32imm:$cc, I32:$arg1, bb_op:$dst),
       [(JVMbrconstcc JVM_CC_GE:$cc , I32:$arg1, bb:$dst)] ,"ifge\t$dst", 0x9c>;
def IFLT : I<(outs), (ins i32imm:$cc, I32:$arg1, bb_op:$dst),
       [(JVMbrconstcc JVM_CC_LT:$cc , I32:$arg1, bb:$dst)] ,"iflt\t$dst", 0x9b>;
def IFLE : I<(outs), (ins i32imm:$cc, I32:$arg1, bb_op:$dst),
       [(JVMbrconstcc JVM_CC_LE:$cc , I32:$arg1, bb:$dst)] ,"ifle\t$dst", 0x9e>;
}

let isTerminator = 1 , isBranch = 1 in  {
def IFICMPEQ : I<(outs), (ins i32imm:$cc, I32:$arg1, I32:$arg2, bb_op:$dst),
       [(JVMbrcc JVM_CC_EQ:$cc , I32:$arg1, I32:$arg2 , bb:$dst)] ,"if_icmpeq\t$dst", 0xa1>;
def IFICMPNE : I<(outs), (ins i32imm:$cc, I32:$arg1, I32:$arg2, bb_op:$dst),
       [(JVMbrcc JVM_CC_NE:$cc , I32:$arg1, I32:$arg2 , bb:$dst)] ,"if_icmpne\t$dst", 0xa1>;
def IFICMPLT : I<(outs), (ins i32imm:$cc, I32:$arg1, I32:$arg2, bb_op:$dst),
       [(JVMbrcc JVM_CC_LT:$cc , I32:$arg1, I32:$arg2 , bb:$dst)] ,"if_icmplt\t$dst", 0xa3>;
def IFICMPLE : I<(outs), (ins i32imm:$cc, I32:$arg1, I32:$arg2, bb_op:$dst),
       [(JVMbrcc JVM_CC_LE:$cc , I32:$arg1, I32:$arg2 , bb:$dst)] ,"if_icmple\t$dst", 0xa3>;
def IFICMPGT : I<(outs), (ins i32imm:$cc, I32:$arg1, I32:$arg2, bb_op:$dst),
       [(JVMbrcc JVM_CC_GT:$cc , I32:$arg1, I32:$arg2 , bb:$dst)] ,"if_icmpgt\t$dst", 0xa3>;
def IFICMPGE : I<(outs), (ins i32imm:$cc, I32:$arg1, I32:$arg2, bb_op:$dst),
       [(JVMbrcc JVM_CC_GE:$cc , I32:$arg1, I32:$arg2 , bb:$dst)] ,"if_icmpge\t$dst", 0xa3>;
def GOTO : I<(outs), (ins bb_op:$dst),
       [(br bb:$dst)] ,"goto\t$dst", 0xa7>;
}

