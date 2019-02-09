//==-- JVMTargetStreamer.h - JVM Target Streamer -*- C++ -*-==//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
///
/// \file
/// \brief This file declares JVM-specific target streamer classes.
/// These are for implementing support for target-specific assembly directives.
///
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_JVM_MCTARGETDESC_JVMTARGETSTREAMER_H
#define LLVM_LIB_TARGET_JVM_MCTARGETDESC_JVMTARGETSTREAMER_H

#include "llvm/ADT/Twine.h"
#include "llvm/Support/MachineValueType.h"
#include "llvm/MC/MCStreamer.h"
#include "llvm/Support/FormattedStream.h"

namespace llvm {

/// JVM-specific streamer interface, to implement support
/// JVM-specific assembly directives.
class JVMTargetStreamer : public MCTargetStreamer {
public:
  explicit JVMTargetStreamer(MCStreamer &S);

  virtual void flush() = 0;

  virtual void emitString(const Twine &name) = 0;
};

/// This part is for ascii assembly output
class JVMTargetAsmStreamer final : public JVMTargetStreamer {
  formatted_raw_ostream &OS;

public:
  void flush() { OS.flush(); }

  JVMTargetAsmStreamer(MCStreamer &S, formatted_raw_ostream &OS);

  void emitString(const Twine &name) override;

  void changeSection(const MCSection *CurSection, MCSection *Section,
                     const MCExpr *SubSection, raw_ostream &OS) override {}
};
} // end namespace llvm

#endif
