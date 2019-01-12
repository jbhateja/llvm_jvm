// JVMInstrMisc.td-JVM Misc codegen support -*- tablegen -*-
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
///
/// \file
/// \brief JVM Miscellaneous operand code-gen constructs.
///
//===----------------------------------------------------------------------===//

multiclass  JVMIntConstants<SDNode node> {  
let mayLoad = 1 , isMoveImm = 1, isAsCheapAsAMove = 1 in {
// Basic load. 
def ICONST_0 : I<(outs I32:$out) , (ins i32imm_op:$arg) ,
                 [(set I32:$out , (node imm0:$arg))] , "iconst_0", 0x3 >;
def ICONST_1 : I<(outs I32:$out) , (ins i32imm_op:$arg) ,
                 [(set I32:$out , (node imm1:$arg))] , "iconst_1", 0x4 >;
def ICONST_2 : I<(outs I32:$out) , (ins i32imm_op:$arg) ,
                 [(set I32:$out , (node imm2:$arg))] , "iconst_2", 0x5 >;
def ICONST_3 : I<(outs I32:$out) , (ins i32imm_op:$arg) ,
                 [(set I32:$out , (node imm3:$arg))] , "iconst_3", 0x6 >;
def ICONST_4 : I<(outs I32:$out) , (ins i32imm_op:$arg) ,
                 [(set I32:$out , (node imm4:$arg))] , "iconst_4", 0x7 >;
def ICONST_5 : I<(outs I32:$out) , (ins i32imm_op:$arg) ,
                 [(set I32:$out , (node imm5:$arg))] , "iconst_5", 0x8 >;

def LCONST_0 : I<(outs I64:$out) , (ins i64imm_op:$arg) ,
                 [(set I64:$out , (node limm0:$arg))] , "lconst_0", 0x9 >;
def LCONST_1 : I<(outs I64:$out) , (ins i64imm_op:$arg) ,
                 [(set I64:$out , (node limm1:$arg))] , "lconst_1", 0xa >;

def LDCI8    : I<(outs I8:$out ), (ins  i8imm_op:$arg),
                 [(set I8:$out ,  (node imm:$arg))], "ldc\t$arg", 0x12>;
def LDCI16   : I<(outs I16:$out ), (ins i16imm_op:$arg),
                 [(set I16:$out , (node imm:$arg))], "ldc\t$arg", 0x12>;
def LDCI32   : I<(outs I32:$out ), (ins i32imm_op:$arg),
                 [(set I32:$out , (node imm:$arg))], "ldc\t$arg", 0x12>;
def LDCI64   : I<(outs I64:$out ), (ins i64imm_op:$arg),
                 [(set I64:$out , (node imm:$arg))], "ldc2_w\t$arg", 0x12>;
} // mayLoad
}

defm CST   : JVMIntConstants<jvmiload>;
defm CSTWC : JVMIntConstants<jvmiloadwch>;


let mayLoad = 1 , isMoveImm = 1, isAsCheapAsAMove = 1 in {
// Basic load. 
def FCONST_0 : I<(outs F32:$out) , (ins f32imm_op:$arg) ,
                 [(set F32:$out , (jvmfload fimm0:$arg))] , "fconst_0", 0xb >;
def FCONST_1 : I<(outs F32:$out) , (ins f32imm_op:$arg) ,
                 [(set F32:$out , (jvmfload fimm1:$arg))] , "fconst_1", 0xc >;
def DCONST_0 : I<(outs F64:$out) , (ins f64imm_op:$arg) ,
                 [(set F64:$out , (jvmfload dimm0:$arg))] , "dconst_0", 0xd >;
def DCONST_1 : I<(outs F64:$out) , (ins f64imm_op:$arg) ,
                 [(set F64:$out , (jvmfload dimm1:$arg))] , "dconst_1", 0xe >;
def LDCF32   : I<(outs F32:$out ), (ins f32imm_op:$arg),
                 [(set F32:$out , (jvmfload fpimm:$arg))], "ldc\t$arg", 0x12>;
def LDCF64   : I<(outs F64:$out ), (ins f64imm_op:$arg),
                 [(set F64:$out , (jvmfload fpimm:$arg))], "ldc2_w\t$arg", 0x12>;
} // mayLoad


let isConsumeBeforeMC = 1, isMoveImm = 1, isReMaterializable = 1 in {
def CONST_INT8 : I<(outs I8:$out), (ins i8imm_op:$arg) ,
                 [(set I8:$out, (imm:$arg))] , "", -1>;
def CONST_INT16 : I<(outs I16:$out), (ins i16imm_op:$arg) ,
                 [(set I16:$out, (imm:$arg))] , "", -1>;
def CONST_INT32 : I<(outs I32:$out), (ins i32imm_op:$arg) ,
                 [(set I32:$out, (imm:$arg))] , "", -1>;
def CONST_INT64 : I<(outs I64:$out), (ins i64imm_op:$arg) ,
                 [(set I64:$out, (imm:$arg))] , "", -1>;
def CONST_FLOAT32 : I<(outs F32:$out), (ins f32imm_op:$arg) ,
                 [(set F32:$out, (fpimm:$arg))] , "", -1>;
def CONST_FLOAT64 : I<(outs F64:$out), (ins f64imm_op:$arg) ,
                 [(set F64:$out, (fpimm:$arg))] , "", -1>;
}

def DUP : I<(outs I32:$out), (ins) , [] , "dup" , 0x59 >;  
def NOP : I<(outs), (ins) , [] , "" >;

let mayLoad = 1 in {
def DUPWCH   : I<(outs I32:$out), (ins I32:$inp) , 
                  [(set I32:$out, (jvmdup I32:$inp))],
                   "dup", 0x59>;  
def INVOKENV : I<(outs I32:$out) , (ins I32:$inp , desc_op:$desc) , 
                  [(set I32:$out, (jvminvokenv I32:$inp ,imm32bit:$desc))], 
                   "invokenonvirtual\t$desc", 0xb7>;
}

multiclass JVMNewInst<IntImmLeaf isArray , IntImmLeaf isClass, string asm, bits<8> opcode> {
 def _CSZ : I<(outs I32:$out), (ins i32imm_op:$sz, desc_op:$desc, i32imm_op:$isarr, i32imm_op:$isclass),
      [(set I32:$out, (jvmnewalloc imm32bit:$sz, imm32bit:$desc, isArray:$isarr, isClass:$isclass))],
      !strconcat(asm,"\t$desc"), opcode>;

 def _VSZ : I<(outs I32:$out), (ins I32:$sz, desc_op:$desc, i32imm_op:$isarr, i32imm_op:$isclass),
      [(set I32:$out, (jvmnewalloc I32:$sz, imm32bit:$desc, isArray:$isarr, isClass:$isclass))],
      !strconcat(asm,"\t$desc"), opcode>;
}

let hasSideEffects = 1 in {
defm PRIMALLOC  : JVMNewInst<imm0, imm0, "", 0x00>;
defm NEWCLASS   : JVMNewInst<imm0, imm1, "new", 0xbb>;
defm NEWARRAY   : JVMNewInst<imm1, imm0, "newarray", 0xbc>;
defm ANEWARRAY  : JVMNewInst<imm1, imm1, "anewarray", 0xbd>;
}

let mayLoad = 1 in {
def EXTENDI82I32  : I<(outs I32:$out), (ins I8:$inp), 
              [(set I32:$out, (jvmext I8:$inp))], "iload\t$inp", 0x15>;
def EXTENDI162I32  : I<(outs I32:$out), (ins I16:$inp), 
              [(set I32:$out, (jvmext I16:$inp))], "iload\t$inp", 0x15>;
def EXTENDI82I64  : I<(outs I64:$out), (ins I8:$inp), 
              [(set I64:$out, (jvmext I8:$inp))], "lload\t$inp", 0x16>;
def EXTENDI162I64  : I<(outs I64:$out), (ins I16:$inp), 
              [(set I64:$out, (jvmext I16:$inp))], "lload\t$inp", 0x16>;
}

let mayLoad = 1 , isMoveImm = 1, isAsCheapAsAMove = 1 in {
def EXTENDCI82I32  : I<(outs I32:$out), (ins i8imm_op:$inp), 
              [(set I32:$out, (jvmext imm8bit:$inp))], "ldc\t$inp", 0x12>;
def EXTENDCI162I32  : I<(outs I32:$out), (ins i16imm_op:$inp), 
              [(set I32:$out, (jvmext imm16bit:$inp))], "ldc\t$inp", 0x12>;
def EXTENDCI82I64  : I<(outs I64:$out), (ins i8imm_op:$inp), 
              [(set I64:$out, (jvmext imm8bit:$inp))], "ldc\t$inp", 0x12>;
def EXTENDCI162I64  : I<(outs I64:$out), (ins i16imm_op:$inp), 
              [(set I64:$out, (jvmext imm16bit:$inp))], "ldc\t$inp", 0x12>;
def EXTENDCI322I64  : I<(outs I64:$out), (ins i32imm_op:$inp), 
              [(set I64:$out, (jvmext imm32bit:$inp))], "ldc2_w\t$inp", 0x12>;
}

def I2C     : I<(outs I16:$out) , (ins I32:$inp),
              [(set I16:$out, (jvmtrunc I32:$inp))], "i2c", 0x92>;
def I32ToB  : I<(outs I8:$out) , (ins I32:$inp),
              [(set I8:$out, (jvmtrunc I32:$inp))], "i2b", 0x91>;
def I16ToB  : I<(outs I8:$out) , (ins I16:$inp),
              [(set I8:$out, (jvmtrunc I16:$inp))], "i2b", 0x91>;
def I2L     : I<(outs I64:$out), (ins I32:$inp), 
              [(set I64:$out, (jvmext I32:$inp))], "i2l", 0x85>; 
def L2I     : I<(outs I32:$out), (ins I64:$inp), 
              [(set I32:$out, (jvmtrunc I64:$inp))], "l2i", 0x88>; 

let mayLoad = 1, isRegCConv = 1 in {
def RCC8to16  : I<(outs I16:$out), (ins I8:$inp), [], "">;
def RCC8to32  : I<(outs I32:$out), (ins I8:$inp), [], "">;
def RCC8to64  : I<(outs I64:$out), (ins I8:$inp), [], "">;
def RCC16to32 : I<(outs I32:$out), (ins I16:$inp), [], "">;
def RCC16to64 : I<(outs I64:$out), (ins I16:$inp), [], "">;
def RCC64to8  : I<(outs I8:$out), (ins I64:$inp), [], "">;
def RCC64to16 : I<(outs I16:$out), (ins I64:$inp), [], "">;
def RCC32to16 : I<(outs I16:$out), (ins I32:$inp), [], "">;
def RCC32to8  : I<(outs I8:$out), (ins I32:$inp), [], "">;
def RCC16to8  : I<(outs I8:$out), (ins I16:$inp), [], "">;
def RCC32to64 : I<(outs I64:$out), (ins I32:$inp), [], "">;
} 



