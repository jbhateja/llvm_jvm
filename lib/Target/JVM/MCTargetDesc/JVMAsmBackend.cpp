//===-- JVMAsmBackend.cpp - JVM Assembler Backend ---------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
///
/// \file
/// \brief This file implements the JVMAsmBackend class.
///
//===----------------------------------------------------------------------===//

#include "MCTargetDesc/JVMMCTargetDesc.h"
#include "llvm/MC/MCAsmBackend.h"
#include "llvm/MC/MCAssembler.h"
#include "llvm/MC/MCDirectives.h"
#include "llvm/MC/MCELFObjectWriter.h"
#include "llvm/MC/MCExpr.h"
#include "llvm/MC/MCFixupKindInfo.h"
#include "llvm/MC/MCObjectWriter.h"
#include "llvm/MC/MCSubtargetInfo.h"
#include "llvm/MC/MCSymbol.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/raw_ostream.h"

using namespace llvm;

namespace {
class JVMAsmBackend final : public MCAsmBackend {
  bool Is64Bit;

public:
  explicit JVMAsmBackend(bool Is64Bit) : MCAsmBackend(support::big), Is64Bit(Is64Bit) {}
  ~JVMAsmBackend() override {}

  std::unique_ptr<MCObjectTargetWriter>
  createObjectTargetWriter() const override;

  unsigned getNumFixupKinds() const override;

  void applyFixup(const MCAssembler &Asm, const MCFixup &Fixup,
                  const MCValue &Target, MutableArrayRef<char> Data,
                  uint64_t Value, bool IsResolved,
                  const MCSubtargetInfo *STI) const override;

  bool mayNeedRelaxation(const MCInst &Inst, const MCSubtargetInfo &STI) const override;

  bool fixupNeedsRelaxation(const MCFixup &Fixup, uint64_t Value,
                            const MCRelaxableFragment *DF,
                            const MCAsmLayout &Layout) const override;

  void relaxInstruction(const MCInst &Inst, const MCSubtargetInfo &STI,
                        MCInst &Res) const override;

  bool writeNopData(raw_ostream &OS, uint64_t Count) const override;
};

std::unique_ptr<MCObjectTargetWriter>
JVMAsmBackend::createObjectTargetWriter() const {
  return nullptr;
}

unsigned JVMAsmBackend::getNumFixupKinds() const { return 0; }

void JVMAsmBackend::applyFixup(const MCAssembler &Asm, const MCFixup &Fixup,
                               const MCValue &Target,
                               MutableArrayRef<char> Data, uint64_t Value,
                               bool IsResolved, const MCSubtargetInfo *STI) const {
  return;
}

bool JVMAsmBackend::mayNeedRelaxation(const MCInst &Inst, const MCSubtargetInfo &STI) const {
  return false;
}

bool JVMAsmBackend::fixupNeedsRelaxation(const MCFixup &Fixup, uint64_t Value,
                                         const MCRelaxableFragment *DF,
                                         const MCAsmLayout &Layout) const {
  return false;
}

void JVMAsmBackend::relaxInstruction(const MCInst &Inst,
                                     const MCSubtargetInfo &STI,
                                     MCInst &Res) const {
  return;
}

bool JVMAsmBackend::writeNopData(raw_ostream &OS, uint64_t Count) const {
  return false;
}
} // end namespace

namespace llvm {
MCAsmBackend * createJVMAsmBackend(const Triple &TT) {
  return new JVMAsmBackend(TT.isArch64Bit());
}
} // namespace llvm
