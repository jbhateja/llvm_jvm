//===-- JVMMCTargetDesc.cpp - JVM Target Descriptions -----===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
///
/// \file
/// \brief This file provides JVM-specific target descriptions.
///
//===----------------------------------------------------------------------===//

#include "JVMMCTargetDesc.h"
#include "InstPrinter/JVMInstPrinter.h"
#include "JVMMCAsmInfo.h"
#include "JVMTargetStreamer.h"
#include "llvm/MC/MCInstrInfo.h"
#include "llvm/MC/MCRegisterInfo.h"
#include "llvm/MC/MCSubtargetInfo.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/TargetRegistry.h"
using namespace llvm;

#define DEBUG_TYPE "jvm-mc-target-desc"

#define GET_INSTRINFO_MC_DESC
#include "JVMGenInstrInfo.inc"

#define GET_SUBTARGETINFO_MC_DESC
#include "JVMGenSubtargetInfo.inc"

#define GET_REGINFO_MC_DESC
#include "JVMGenRegisterInfo.inc"

static MCAsmInfo *createMCAsmInfo(const MCRegisterInfo & /*MRI*/,
                                  const Triple &TT) {
  return new JVMMCAsmInfo(TT);
}

static MCInstrInfo *createMCInstrInfo() {
  MCInstrInfo *X = new MCInstrInfo();
  InitJVMMCInstrInfo(X);
  return X;
}

static MCRegisterInfo *createMCRegisterInfo(const Triple & /*T*/) {
  MCRegisterInfo *X = new MCRegisterInfo();
  InitJVMMCRegisterInfo(X, 0);
  return X;
}

static MCInstPrinter *createMCInstPrinter(const Triple & /*T*/,
                                          unsigned SyntaxVariant,
                                          const MCAsmInfo &MAI,
                                          const MCInstrInfo &MII,
                                          const MCRegisterInfo &MRI) {
  assert(SyntaxVariant == 0 && "JVM only has one syntax variant");
  return new JVMInstPrinter(MAI, MII, MRI);
}

static MCAsmBackend *createAsmBackend(const Target & /*T*/,
                                      const MCSubtargetInfo &STI,
                                      const MCRegisterInfo & /*MRI*/,
                                      const MCTargetOptions & /*Options*/) {
  return createJVMAsmBackend(STI.getTargetTriple());
}

static MCSubtargetInfo *createMCSubtargetInfo(const Triple &TT, StringRef CPU,
                                              StringRef FS) {
  return createJVMMCSubtargetInfoImpl(TT, CPU, FS);
}

static MCTargetStreamer *createAsmTargetStreamer(MCStreamer &S,
                                                 formatted_raw_ostream &OS,
                                                 MCInstPrinter * /*InstPrint*/,
                                                 bool /*isVerboseAsm*/) {
  return new JVMTargetAsmStreamer(S, OS);
}

// Force static initialization.
extern "C" void LLVMInitializeJVMTargetMC() {
  for (Target *T : {&getTheJVMTarget32(), &getTheJVMTarget64()}) {
    // Register the MC asm info.
    RegisterMCAsmInfoFn X(*T, createMCAsmInfo);

    // Register the MC instruction info.
    TargetRegistry::RegisterMCInstrInfo(*T, createMCInstrInfo);

    // Register the MC register info.
    TargetRegistry::RegisterMCRegInfo(*T, createMCRegisterInfo);

    // Register the MCInstPrinter.
    TargetRegistry::RegisterMCInstPrinter(*T, createMCInstPrinter);

    // Register the ASM Backend.
    TargetRegistry::RegisterMCAsmBackend(*T, createAsmBackend);

    // Register the MC subtarget info.
    TargetRegistry::RegisterMCSubtargetInfo(*T, createMCSubtargetInfo);

    // Register the asm target streamer.
    TargetRegistry::RegisterAsmTargetStreamer(*T, createAsmTargetStreamer);
  }
}
