// JVMFrameLowering.h - TargetFrameLowering for JVM -*- C++ -*-/
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
///
/// \file
/// \brief This class implements JVM-specific bits of
/// TargetFrameLowering class.
///
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_JVM_JVMFRAMELOWERING_H
#define LLVM_LIB_TARGET_JVM_JVMFRAMELOWERING_H

#include "llvm/CodeGen/TargetFrameLowering.h"

namespace llvm {
class MachineFrameInfo;

class JVMFrameLowering final : public TargetFrameLowering {
public:
  JVMFrameLowering()
      : TargetFrameLowering(StackGrowsDown, /*StackAlignment=*/16,
                            /*LocalAreaOffset=*/0,
                            /*TransientStackAlignment=*/16,
                            /*StackRealignable=*/true) {}

  bool hasFP(const MachineFunction &MF) const override;

  /// These methods insert prolog and epilog code into the function.
  void emitPrologue(MachineFunction &MF, MachineBasicBlock &MBB) const override;
  void emitEpilogue(MachineFunction &MF, MachineBasicBlock &MBB) const override;
};
} // end namespace llvm

#endif
