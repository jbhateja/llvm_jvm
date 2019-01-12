//JVMRegisterInfo.td-Describe the JVM Registers -*- tablegen -*-
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
///
/// \file
/// \brief This file describes the JVM register classes and some nominal
/// physical registers.
///
//===----------------------------------------------------------------------===//

class JVMReg<string n> : Register<n> {
  let Namespace = "JVM";
}

class JVMRegClass<list<ValueType> regTypes, int alignment, dag regList>
     : RegisterClass<"JVM", regTypes, alignment, regList>;

//===----------------------------------------------------------------------===//
// Registers
//===----------------------------------------------------------------------===//

// JVM is a stack-based virtual machine, does not have any registers
// Following integral and floating point registers are helpful in creating
// generic register classes useful mainly while pattern matching.

def I8Reg  : JVMReg<"%I8">;
def I16Reg : JVMReg<"%I16">;
def I32Reg : JVMReg<"%I32">;
def I64Reg : JVMReg<"%I64">;
def F32Reg : JVMReg<"%F32">;
def F64Reg : JVMReg<"%F64">;

// The incoming arguments "register". This is an opaque entity which serves to
// order the ARGUMENT instructions that are emulating live-in registers and
// must not be scheduled below other instructions.
def ARGUMENTS : JVMReg<"ARGUMENTS">;

//===----------------------------------------------------------------------===//
//  Register classes
//===----------------------------------------------------------------------===//

def I8  : JVMRegClass<[i8] , 8,  (add I8Reg)>;
def I16 : JVMRegClass<[i16], 16, (add I16Reg)>;
def I32 : JVMRegClass<[i32], 32, (add I32Reg)>;
def I64 : JVMRegClass<[i64], 64, (add I64Reg)>;
def F32 : JVMRegClass<[f32], 32, (add F32Reg)>;
def F64 : JVMRegClass<[f64], 64, (add F64Reg)>;

