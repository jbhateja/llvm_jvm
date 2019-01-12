// JVMInstrMemory.td-JVM Memory codegen support -*- tablegen -*-
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
///
/// \file
/// \brief JVM Memory operand code-gen constructs.
///
//===----------------------------------------------------------------------===//

// Defines atomic and non-atomic loads, regular and extending.
class JVMLoad<JVMRegClass rc, SDNode Node, string Name, int Opcode, list<Predicate> preds> :
  I<(outs rc:$out), (ins rc:$addr),
    [(set rc:$out , (Node rc:$addr))], !strconcat(Name, "\t$addr"), Opcode, preds>;

def objaccr : PatFrag <(outs node:$out), (jvmiloadwch node:$out), [{ 
  return N->getFlags().hasIsComposite() &&
        !N->getFlags().hasIsArrayAcc() && 
        !N->getFlags().hasIsStructAcc();
}]>;


def iarraccr : PatFrag<(outs node:$out), (jvmiloadwch node:$out), [{
  return !N->getFlags().hasIsComposite() &&
          N->getFlags().hasIsArrayAcc() &&
         !N->getFlags().hasIsStructAcc();
}]>;

def aarraccr : PatFrag<(outs node:$out), (jvmiloadwch node:$out) , [{
  return N->getFlags().hasIsComposite() &&
         N->getFlags().hasIsArrayAcc() &&
        !N->getFlags().hasIsStructAcc();
}]>;

def objaccw : PatFrag <(outs node:$src), (jvmistoreres node:$src), [{ 
  return N->getFlags().hasIsComposite() &&
        !N->getFlags().hasIsArrayAcc() && 
        !N->getFlags().hasIsStructAcc();
}]>;

def aarraccw : PatFrag<(outs node:$src, node:$dst), (jvmistore node:$src, node:$dst), [{
  return N->getFlags().hasIsComposite() && 
         N->getFlags().hasIsArrayAcc() && 
        !N->getFlags().hasIsStructAcc();
}]>;

def iaarraccw : PatFrag<(outs node:$src, node:$dst), (jvmistore node:$src, node:$dst), [{
  return !N->getFlags().hasIsComposite() && 
          N->getFlags().hasIsArrayAcc() && 
         !N->getFlags().hasIsStructAcc();
}]>;

def arridxbipush : PatFrag<(outs node:$arr, node:$idx), (jvmidxacc node:$arr, node:$idx), [{
  SDNode * Index = N->getOperand(2).getNode();
  return isa<ConstantSDNode>(Index) && (cast<ConstantSDNode>(Index)->getZExtValue() < 256);
}]>;

def arridxsipush : PatFrag<(outs node:$arr, node:$idx), (jvmidxacc node:$arr, node:$idx), [{
  SDNode * Index = N->getOperand(2).getNode();
  return isa<ConstantSDNode>(Index) && (cast<ConstantSDNode>(Index)->getZExtValue() >= 256);
}]>;

def arridxiload : PatFrag<(outs node:$arr, node:$idx), (jvmidxacc node:$arr, node:$idx), [{
  SDNode * Index = N->getOperand(2).getNode();
  return !isa<ConstantSDNode>(Index);
}]>;
   

// Array index instructions.
let mayLoad = 1 in {
def BIPUSHIDX8 : I<(outs I32:$out) , (ins I32:$arr, i8imm:$idx),
    [(set I32:$out, (arridxbipush I32:$arr, imm8bit:$idx))], "bipush\t$idx", 0x10, []>;

def BIPUSHIDX16: I<(outs I32:$out) , (ins I32:$arr, i16imm:$idx),
    [(set I32:$out, (arridxbipush I32:$arr, imm16bit:$idx))], "bipush\t$idx", 0x10, []>;

def BIPUSHIDX32 : I<(outs I32:$out) , (ins I32:$arr, i32imm:$idx),
    [(set I32:$out, (arridxbipush I32:$arr, imm32bit:$idx))], "bipush\t$idx", 0x10, []>;

def BIPUSHIDX64 : I<(outs I32:$out) , (ins I32:$arr, i64imm:$idx),
    [(set I32:$out, (arridxbipush I32:$arr, imm64bit:$idx))], "bipush\t$idx", 0x10, []>;

def SIPUSHIDX16: I<(outs I32:$out) , (ins I32:$arr, i16imm:$idx),
    [(set I32:$out, (arridxsipush I32:$arr, imm16bit:$idx))], "sipush\t$idx", 0x10, []>;

def SIPUSHIDX32 : I<(outs I32:$out), (ins I32:$arr, i32imm:$idx),
    [(set I32:$out, (arridxsipush I32:$arr, imm32bit:$idx))], "sipush\t$idx", 0x11, []>;

def SIPUSHIDX64 : I<(outs I32:$out), (ins I32:$arr, i64imm:$idx),
    [(set I32:$out, (arridxsipush I32:$arr, imm64bit:$idx))], "sipush\t$idx", 0x11, []>;

def ILOAD32_ARR_IDX8 : I<(outs I32:$out), (ins I32:$arr, I8:$idx),
    [(set I32:$out, (arridxiload I32:$arr, I8:$idx))], "iload\t$idx", 0x15, []>;

def ILOAD32_ARR_IDX16 : I<(outs I32:$out), (ins I32:$arr, I16:$idx),
    [(set I32:$out, (arridxiload I32:$arr, I16:$idx))], "iload\t$idx", 0x15, []>;

def ILOAD32_ARR_IDX32 : I<(outs I32:$out), (ins I32:$arr, I32:$idx),
    [(set I32:$out, (arridxiload I32:$arr, I32:$idx))], "iload\t$idx", 0x15, []>;

}

let mayLoad = 1 in {
// Basic load.
def ILOAD8     : JVMLoad<I8,  jvmiload, "iload", 0x15, []>;
def ILOAD16    : JVMLoad<I16, jvmiload, "iload", 0x15, []>;
def ILOAD32    : JVMLoad<I32, jvmiload, "iload", 0x15, []>;
def ILOAD64    : JVMLoad<I64, jvmiload, "lload", 0x16, []>;
def ILOAD8WCH  : JVMLoad<I8,  jvmiloadwch, "iload", 0x15, []>;
def ILOAD16WCH : JVMLoad<I16, jvmiloadwch, "iload", 0x15, []>;
def ILOAD32WCH : JVMLoad<I32, jvmiloadwch, "iload", 0x15, []>;
def ILOAD64WCH : JVMLoad<I64, jvmiloadwch, "lload", 0x16, []>;

def ALOAD   : I<(outs I32:$out), (ins I32:$addr),
    [(set I32:$out , (objaccr I32:$addr))], "aload\t$addr", 0x19, []>;

def I8ALOAD: I<(outs I8:$out), (ins I8:$addr),
    [(set I8:$out, (iarraccr I8:$addr))], "baload", 0x33, []>;

def I16ALOAD: I<(outs I16:$out), (ins I16:$addr),
    [(set I16:$out, (iarraccr I16:$addr))], "saload", 0x35, []>;

def I32ALOAD: I<(outs I32:$out), (ins I32:$addr),
    [(set I32:$out, (iarraccr I32:$addr))], "iaload", 0x2e, []>;
 
def I64ALOAD: I<(outs I64:$out), (ins I64:$addr),
    [(set I64:$out, (iarraccr I64:$addr))], "laload", 0x2f, []>;

def AALOAD: I<(outs I32:$out), (ins I32:$addr),
    [(set I32:$out, (aarraccr I32:$addr))], "aaload", 0x32, []>;


def GETFIELD8 : I<(outs I8:$out), (ins I32:$obj , desc_op:$desc),
    [(set I8:$out, (jvmfldaccr I32:$obj , imm32bit:$desc))], "getfield\t$desc", 0xb4, []>;

def GETFIELD16 : I<(outs I16:$out), (ins I32:$obj , desc_op:$desc),
    [(set I16:$out, (jvmfldaccr I32:$obj , imm32bit:$desc))], "getfield\t$desc", 0xb4, []>;

def GETFIELD32 : I<(outs I32:$out), (ins I32:$obj , desc_op:$desc),
    [(set I32:$out, (jvmfldaccr I32:$obj , imm32bit:$desc))], "getfield\t$desc", 0xb4, []>;
  
def GETFIELD64 : I<(outs I64:$out), (ins I32:$obj , desc_op:$desc),
    [(set I64:$out, (jvmfldaccr I32:$obj , imm32bit:$desc))], "getfield\t$desc", 0xb4, []>;

} // mayLoad


let mayStore = 1 , isConsumeBeforeMC = 1, hasCtrlDep = 1 in {
// Basic store.
def ISTORE8NORES  : I<(outs), (ins I8:$src, I32:$dst), 
                       [(jvmistore I8:$src, I32:$dst)],
                       "istore\t$dst", 0x36>;

def ISTORE16NORES : I<(outs), (ins I16:$src, I32:$dst), 
                       [(jvmistore I16:$src, I32:$dst)],
                       "istore\t$dst", 0x36>;

def ISTORE32NORES : I<(outs), (ins I32:$src, I32:$dst), 
                       [(jvmistore I32:$src, I32:$dst)],
                       "istore\t$dst", 0x36>;

def ISTORE64NORES : I<(outs), (ins I64:$src, I32:$dst), 
                       [(jvmistore I64:$src, I32:$dst)],
                       "lstore\t$dst", 0x37>;
}

let mayStore = 1 , hasCtrlDep = 1 in {
def ISTORE8   : I<(outs I8:$dst), (ins I8:$src),
                   [(set I8:$dst, (jvmistoreres I8:$src))],
                   "istore\t$dst", 0x36>;

def ISTORE16  : I<(outs I16:$dst), (ins I16:$src),
                   [(set I16:$dst, (jvmistoreres I16:$src))],
                   "istore\t$dst", 0x36>;

def ISTORE32  : I<(outs I32:$dst), (ins I32:$src),
                   [(set I32:$dst, (jvmistoreres I32:$src))],
                   "istore\t$dst", 0x36>;
def ISTORE64  : I<(outs I64:$dst), (ins I64:$src),
                   [(set I64:$dst, (jvmistoreres I64:$src))],
                   "lstore\t$dst", 0x37>;

def ASTORE    : I<(outs I32:$dst), (ins I32:$src),
                   [(set I32:$dst, (objaccw I32:$src))],
                   "astore\t$dst", 0x3a>;

def PUTFIELD8  : I<(outs), (ins I32:$obj , I8:$src, desc_op:$desc),
    [(jvmfldaccw I32:$obj , I8:$src, imm32bit:$desc)], "putfield\t$desc", 0xb5, []>;

def PUTFIELD16 : I<(outs), (ins I32:$obj , I16:$src, desc_op:$desc),
    [(jvmfldaccw I32:$obj , I16:$src, imm32bit:$desc)], "putfield\t$desc", 0xb5, []>;

def PUTFIELD32 : I<(outs), (ins I32:$obj , I32:$src, desc_op:$desc),
    [(jvmfldaccw I32:$obj , I32:$src, imm32bit:$desc)], "putfield\t$desc", 0xb5, []>;

def PUTFIELD64 : I<(outs), (ins I32:$obj , I64:$src, desc_op:$desc),
    [(jvmfldaccw I32:$obj , I64:$src, imm32bit:$desc)], "putfield\t$desc", 0xb5, []>;

def AASTORE : I<(outs) , (ins I32:$src, I32:$dst) , 
    [(aarraccw I32:$src, I32:$dst)] , "aastore", 0x53, []>;

def IASTORE8 : I<(outs), (ins I8:$src, I32:$dst) ,
    [( iaarraccw I8:$src, I32:$dst)], "bastore", 0x54, []>;

def IASTORE16 : I<(outs), (ins I16:$src, I32:$dst) ,
    [( iaarraccw I16:$src, I32:$dst)], "sastore", 0x56, []>;

def IASTORE32 : I<(outs), (ins I32:$src, I32:$dst) ,
    [( iaarraccw I32:$src, I32:$dst)], "iastore", 0x4f, []>;

def IASTORE64 : I<(outs), (ins I64:$src, I32:$dst),
    [( iaarraccw I64:$src, I32:$dst)], "lastore", 0x50, []>;
}

