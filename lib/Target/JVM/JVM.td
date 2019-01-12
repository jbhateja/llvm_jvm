//- JVM.td - Describe the JVM Target Machine --*- tablegen -*-//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
///
/// \file
/// \brief This is a target description file for the JVM architecture,
/// which is also known as "jvm".
///
//===----------------------------------------------------------------------===//

//===----------------------------------------------------------------------===//
// Target-independent interfaces which we are implementing
//===----------------------------------------------------------------------===//

include "llvm/Target/Target.td"

//===----------------------------------------------------------------------===//
// Architectures.
//===----------------------------------------------------------------------===//

//===----------------------------------------------------------------------===//
// Register File Description
//===----------------------------------------------------------------------===//

include "JVMRegisterInfo.td"

//===----------------------------------------------------------------------===//
// Instruction Descriptions
//===----------------------------------------------------------------------===//

include "JVMInstrInfo.td"

def JVMInstrInfo : InstrInfo;

//===----------------------------------------------------------------------===//
// JVM Processors supported.
//===----------------------------------------------------------------------===//

// Minimal Viable Product.
def : ProcessorModel<"jvm", NoSchedModel, []>;

//===----------------------------------------------------------------------===//
// Target Declaration
//===----------------------------------------------------------------------===//

def JVM : Target {
  let InstructionSet = JVMInstrInfo;
}
