//===-- JVMInstrInfo.cpp - JVM Instruction Information ----===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
///
/// \file
/// \brief This file contains the JVM implementation of the
/// TargetInstrInfo class.
///
//===----------------------------------------------------------------------===//

#include "JVMInstrInfo.h"
#include "JVMMachineFunctionInfo.h"
#include "JVMSubtarget.h"
#include "MCTargetDesc/JVMMCTargetDesc.h"
#include "llvm/CodeGen/MachineFrameInfo.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/MachineMemOperand.h"
#include "llvm/CodeGen/MachineRegisterInfo.h"
using namespace llvm;

#define DEBUG_TYPE "jvm-instr-info"

#define GET_INSTRINFO_CTOR_DTOR
#include "JVMGenInstrInfo.inc"

JVMInstrInfo::JVMInstrInfo(const JVMSubtarget &STI)
    : JVMGenInstrInfo(), RI(STI.getTargetTriple()) {}

void JVMInstrInfo::copyPhysReg(MachineBasicBlock &MBB,
                               MachineBasicBlock::iterator I,
                               const DebugLoc &DL, unsigned DestReg,
                               unsigned SrcReg, bool KillSrc) const {
  llvm_unreachable("Unsupported copyPhysReg.");
}


bool JVMInstrInfo::IsConditionalBranch(const MachineInstr &MI) const {
  assert(MI.isBranch() && "Branch instruction expected");
  switch (MI.getOpcode()) {
  default:
    return true;
  case JVM::GOTO:
    return false;
  }
}


// Support function used by MachineBranchPlacement.
bool JVMInstrInfo::analyzeBranch(MachineBasicBlock &MBB,
                                 MachineBasicBlock *&TBB,
                                 MachineBasicBlock *&FBB,
                                 SmallVectorImpl<MachineOperand> &Cond,
                                 bool AllowModify) const {
  MachineInstr *UBR = nullptr;
  MachineInstr *CBR = nullptr;
  for (auto ritr = MBB.instr_rbegin(); ritr != MBB.instr_rend(); ritr++) {
    MachineInstr &MI = *ritr;
    if (!MI.isBranch())
      break;
    if (!IsConditionalBranch(MI)) {
      assert(!UBR && "Multiple-unconditional branches");
      UBR = &MI;
    } else {
      assert(!CBR && "Multiple-conditional branches");
      CBR = &MI;
    }
  }

  if (nullptr == CBR && nullptr == UBR)
    return true;
  else if (UBR && !CBR) {
    TBB = UBR->getOperand(0).getMBB();
  } else if (CBR) {
    TBB = CBR->getOperand(CBR->getNumOperands() - 1).getMBB();
    for (unsigned i = 0; i < CBR->getNumOperands() - 2; i++)
      Cond.push_back(CBR->getOperand(i));
    FBB = UBR ? UBR->getOperand(0).getMBB() : nullptr;
  } else {
    llvm_unreachable("Unhandled banching");
  }
  return false;
}


bool JVMInstrInfo::reverseBranchCondition(SmallVectorImpl<MachineOperand> &Cond) const {
  MachineOperand &MO = Cond[0];
  unsigned NewCond;
  switch (MO.getImm()) {
     default:
        llvm_unreachable("Unhandled conditon");
     case JVM::IFICMPEQ:
        NewCond = JVM::IFICMPNE;
        break;
     case JVM::IFICMPNE:
        NewCond = JVM::IFICMPEQ;
        break;
     case JVM::IFICMPGT:
        NewCond = JVM::IFICMPLE;
        break;
     case JVM::IFICMPGE:
        NewCond = JVM::IFICMPLT;
        break;
     case JVM::IFICMPLT:
        NewCond = JVM::IFICMPGE;
        break;
     case JVM::IFICMPLE:
        NewCond = JVM::IFICMPGT;
        break;
     case JVM::IFEQ:
        NewCond = JVM::IFNE;
        break;
     case JVM::IFGT:
        NewCond = JVM::IFLE;
        break;
     case JVM::IFGE:
        NewCond = JVM::IFLT;
        break;
     case JVM::IFLT:
        NewCond = JVM::IFGE;
        break;
     case JVM::IFLE:
        NewCond = JVM::IFGT;
        break;
     case JVM::IFNE:
        NewCond = JVM::IFEQ;
        break;
  }
  Cond[0].setImm(NewCond);
  return false;
}


unsigned JVMInstrInfo::removeBranch(MachineBasicBlock &MBB, 
                                    int *BytesRemoved) const {
  unsigned Count = 0;
  for (auto ritr = MBB.instr_rbegin(); ritr != MBB.instr_rend(); ritr++) {
    MachineInstr &MI = *ritr;
    if (!MI.isBranch())
      break;
    MI.eraseFromParent();
    Count++;
  }
  return Count;
}

MachineBasicBlock *JVMInstrInfo::getBranchDestBlock(const MachineInstr &MI) const {
  if (!MI.isBranch())
    return nullptr;
  else if (!IsConditionalBranch(MI))
    return MI.getOperand(0).getMBB();
  else
    return MI.getOperand(MI.getNumOperands() - 1).getMBB();
}

