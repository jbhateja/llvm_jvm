//==- JVMDisassembler.cpp - Disassembler for JVM -*- C++ -*-==//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
///
/// \file
/// \brief This file is part of the JVM Disassembler.
///
/// It contains code to translate the data produced by the decoder into
/// MCInsts.
///
//===----------------------------------------------------------------------===//

#include "JVM.h"
#include "MCTargetDesc/JVMMCTargetDesc.h"
#include "llvm/MC/MCContext.h"
#include "llvm/MC/MCDisassembler/MCDisassembler.h"
#include "llvm/MC/MCInst.h"
#include "llvm/MC/MCInstrInfo.h"
#include "llvm/MC/MCSubtargetInfo.h"
#include "llvm/MC/MCSymbol.h"
#include "llvm/Support/Endian.h"
#include "llvm/Support/TargetRegistry.h"
using namespace llvm;

#define DEBUG_TYPE "jvm-disassembler"

namespace {
class JVMDisassembler final : public MCDisassembler {
  std::unique_ptr<const MCInstrInfo> MCII;

  DecodeStatus getInstruction(MCInst &Instr, uint64_t &Size,
                              ArrayRef<uint8_t> Bytes, uint64_t Address,
                              raw_ostream &VStream,
                              raw_ostream &CStream) const override;

public:
  JVMDisassembler(const MCSubtargetInfo &STI, MCContext &Ctx,
                  std::unique_ptr<const MCInstrInfo> MCII)
      : MCDisassembler(STI, Ctx), MCII(std::move(MCII)) {}
};
} // end anonymous namespace

static MCDisassembler *createJVMDisassembler(const Target &T,
                                             const MCSubtargetInfo &STI,
                                             MCContext &Ctx) {
  std::unique_ptr<const MCInstrInfo> MCII(T.createMCInstrInfo());
  return new JVMDisassembler(STI, Ctx, std::move(MCII));
}

extern "C" void LLVMInitializeJVMDisassembler() {
  // Register the disassembler for each target.
  TargetRegistry::RegisterMCDisassembler(getTheJVMTarget32(),
                                         createJVMDisassembler);
  TargetRegistry::RegisterMCDisassembler(getTheJVMTarget64(),
                                         createJVMDisassembler);
}

MCDisassembler::DecodeStatus
JVMDisassembler::getInstruction(MCInst &MI, uint64_t &Size,
                                ArrayRef<uint8_t> Bytes, uint64_t /*Address*/,
                                raw_ostream &OS, raw_ostream &CS) const {

  // TODO: Implement disassembly.

  return MCDisassembler::Fail;
}
