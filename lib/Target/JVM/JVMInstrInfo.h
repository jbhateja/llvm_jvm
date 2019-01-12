//=- JVMInstrInfo.h - JVM Instruction Information -*- C++ -*-=//
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

#ifndef LLVM_LIB_TARGET_JVM_JVMINSTRINFO_H
#define LLVM_LIB_TARGET_JVM_JVMINSTRINFO_H

#include "JVMRegisterInfo.h"
#include "llvm/CodeGen/TargetInstrInfo.h"

#define GET_INSTRINFO_HEADER
#include "JVMGenInstrInfo.inc"

namespace llvm {

class JVMSubtarget;

class JVMInstrInfo final : public JVMGenInstrInfo {
  const JVMRegisterInfo RI;

public:
  explicit JVMInstrInfo(const JVMSubtarget &STI);

  const JVMRegisterInfo &getRegisterInfo() const { return RI; }

  void copyPhysReg(MachineBasicBlock &MBB, MachineBasicBlock::iterator MI,
                   const DebugLoc &DL, unsigned DestReg, unsigned SrcReg,
                   bool KillSrc) const override;

  bool analyzeBranch(MachineBasicBlock &MBB, MachineBasicBlock *&TBB,
                     MachineBasicBlock *&FBB,
                     SmallVectorImpl<MachineOperand> &Cond,
                     bool AllowModify = false) const override;

  bool reverseBranchCondition(SmallVectorImpl<MachineOperand> &Cond) const override;

  unsigned removeBranch(MachineBasicBlock &MBB,
                        int *BytesRemoved = nullptr) const override;

  MachineBasicBlock *getBranchDestBlock(const MachineInstr &MI) const override;

  bool analyzeSelect(const MachineInstr &MI,
                     SmallVectorImpl<MachineOperand> &Cond,
                     unsigned &TrueOp, unsigned &FalseOp,
                     bool &Optimizable) const override { return false;}
private:
  bool IsConditionalBranch(const MachineInstr &MI) const;
};
} // end namespace llvm

#endif
