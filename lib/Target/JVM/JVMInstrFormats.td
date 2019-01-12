//=- JVMInstrFormats.td - JVM Instr. Formats -*- tablegen -*-=//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
///
/// \file
/// \brief JVM instruction format definitions.
///
//===----------------------------------------------------------------------===//

// JVM Instruction Format.
class JVMInst<bits<8> inst, string asmstr> : Instruction {
  field bits<8> Inst = inst; // Instruction encoding.
  let Namespace   = "JVM";
  let Pattern     = [];
  let Predicates  = [];
  let AsmString   = asmstr;
}

// Normal instructions.
class I<dag oops, dag iops, list<dag> pattern, string asmstr = "",
        bits<8> inst = -1, list<Predicate> preds = []>
    : JVMInst<inst, asmstr> {
  let OutOperandList = oops;
  let InOperandList  = iops;
  let Pattern        = pattern;
  let Predicates     = preds;
}

// Unary and binary instructions, for the local types that JVM supports.
multiclass UnaryInt<SDNode node, string name, bits<8> i32Inst, bits<8> i64Inst> {
  def _32 : I<(outs I32:$dst), (ins I32:$src),
               [(set I32:$dst, (node I32:$src))],
               !strconcat("i", name), i32Inst>;
  def _64 : I<(outs I64:$dst), (ins I64:$src),
               [(set I64:$dst, (node I64:$src))],
               !strconcat("l", name), i64Inst>;
}
multiclass BinaryInt<SDNode node, string name, bits<8> i32Inst, bits<8> i64Inst> {
  def _8 :  I<(outs I8:$dst), (ins I8:$lhs, I8:$rhs),
               [(set I8:$dst, (node I8:$lhs, I8:$rhs))],
               !strconcat("i", name), i32Inst >;
  def _16 : I<(outs I16:$dst), (ins I16:$lhs, I16:$rhs),
               [(set I16:$dst, (node I16:$lhs, I16:$rhs))],
               !strconcat("i", name), i32Inst >;
  def _32 : I<(outs I32:$dst), (ins I32:$lhs, I32:$rhs),
               [(set I32:$dst, (node I32:$lhs, I32:$rhs))],
               !strconcat("i", name), i32Inst >;
  def _64 : I<(outs I64:$dst), (ins I64:$lhs, I64:$rhs),
               [(set I64:$dst, (node I64:$lhs, I64:$rhs))],
               !strconcat("l", name), i64Inst >;
}

multiclass BinaryIntShift<SDNode node, string name, bits<8> i32Inst, bits<8> i64Inst> {
  def _8 :  I<(outs I8:$dst), (ins I8:$lhs, I32:$rhs),
               [(set I8:$dst, (node I8:$lhs, I32:$rhs))],
               !strconcat("i", name), i32Inst >;
  def _16 : I<(outs I16:$dst), (ins I16:$lhs, I32:$rhs),
               [(set I16:$dst, (node I16:$lhs, I32:$rhs))],
               !strconcat("i", name), i32Inst >;
  def _32 : I<(outs I32:$dst), (ins I32:$lhs, I32:$rhs),
               [(set I32:$dst, (node I32:$lhs, I32:$rhs))],
               !strconcat("i", name), i32Inst >;
  def _64 : I<(outs I64:$dst), (ins I64:$lhs, I32:$rhs),
               [(set I64:$dst, (node I64:$lhs, I32:$rhs))],
               !strconcat("l", name), i64Inst >;
}

multiclass UnaryFP<SDNode node, string name, bits<8> f32Inst, bits<8> f64Inst> {
  def _32 : I<(outs F32:$dst), (ins F32:$src),
               [(set F32:$dst, (node F32:$src))],
               !strconcat("f" ,name), f32Inst>;
  def _64 : I<(outs F64:$dst), (ins F64:$src),
               [(set F64:$dst, (node F64:$src))],
               !strconcat("d", name), f64Inst>;
}
multiclass BinaryFP<SDNode node, string name, bits<8> f32Inst, bits<8> f64Inst> {
  def _32 : I<(outs F32:$dst), (ins F32:$lhs, F32:$rhs),
               [(set F32:$dst, (node F32:$lhs, F32:$rhs))],
               !strconcat("f", name), f32Inst >;
  def _64 : I<(outs F64:$dst), (ins F64:$lhs, F64:$rhs),
               [(set F64:$dst, (node F64:$lhs, F64:$rhs))],
               !strconcat("d", name), f64Inst >;
}


