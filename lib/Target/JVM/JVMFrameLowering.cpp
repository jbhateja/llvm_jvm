//===-- JVMFrameLowering.cpp - JVM Frame Lowering ----------==//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
///
/// \file
/// \brief This file contains the JVM implementation of
/// TargetFrameLowering class.
///
/// On JVM, there aren't a lot of things to do here. There are no
/// callee-saved registers to save, and no spill slots.
///
/// Frame of JVM method has following :
/// a/ Local value array which holds the arguments and intermediate
///    results.
/// b/ Operand stack which is populated at run time.
///
/// Jasmin assembly format needs .limit stack and .limit locals
///
//===----------------------------------------------------------------------===//

#include "JVMFrameLowering.h"
#include "JVMInstrInfo.h"
#include "JVMMachineFunctionInfo.h"
#include "JVMSubtarget.h"
#include "JVMTargetMachine.h"
#include "MCTargetDesc/JVMMCTargetDesc.h"
#include "llvm/CodeGen/MachineFrameInfo.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/MachineModuleInfoImpls.h"
#include "llvm/CodeGen/MachineRegisterInfo.h"
#include "llvm/Support/Debug.h"
using namespace llvm;

#define DEBUG_TYPE "jvm-frame-info"

void JVMFrameLowering::emitPrologue(MachineFunction &MF,
                                    MachineBasicBlock &MBB) const {

  // TODO : Evaluate the frame info and set stack_limit and
  // locals_limit into MachineFunction info, this will be used
  // by AsmPrinter to emit the required .limit directives.
}

void JVMFrameLowering::emitEpilogue(MachineFunction &MF,
                                    MachineBasicBlock &MBB) const {
  // Nothing in perticulare needed for JVM as of now.
}

bool JVMFrameLowering::hasFP(const MachineFunction &MF) const { return false; }
