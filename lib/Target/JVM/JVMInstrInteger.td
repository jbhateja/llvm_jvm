// JVMInstrInteger.td-JVM Integer codegen -------*- tablegen -*-
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
///
/// \file
/// \brief JVM Integer operand code-gen constructs.
///
//===----------------------------------------------------------------------===//

// The spaces after the names are for aesthetic purposes only, to make
// operands line up vertically after tab expansion.
let isCommutable = 1 in {
defm IADD : BinaryInt<iadd, "add ", 0x60, 0x61>;
defm ISUB : BinaryInt<isub, "sub ", 0x64, 0x65>;
defm IMUL : BinaryInt<imul, "mul ", 0x68, 0x69>;
defm IOR  : BinaryInt<ior,  "or"  , 0x80, 0x81>;
defm IXOR : BinaryInt<ixor, "xor" , 0x82, 0x83>;
defm IAND : BinaryInt<iand, "and" , 0x7e, 0x7f>;
}

defm IDIV : BinaryInt<idiv, "div" , 0x6c, 0x6d>;
defm IREM : BinaryInt<irem, "rem" , 0x70, 0x71>;
defm ISHL : BinaryIntShift<ishl, "shl" , 0x78, 0x79>;
defm ISRL : BinaryIntShift<isrl, "ushr" , 0x7c, 0x7d>;
defm ISRA : BinaryIntShift<isra, "shr" , 0x7a, 0x7b>;
                 
// TODO : Add optimizing patterns for implied addressing mode instructions.
