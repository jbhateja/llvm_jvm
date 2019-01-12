//===-- JVMMCInstLower.h - Lower MachineInstr to MCInst -*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
///
/// \file
/// \brief This file declares the class to lower JVM MachineInstrs to
/// their corresponding MCInst records.
///
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_JVM_JVMMCINSTLOWER_H
#define LLVM_LIB_TARGET_JVM_JVMMCINSTLOWER_H

#include "llvm/MC/MCInst.h"
#include "llvm/Support/Compiler.h"

namespace llvm {
class JVMAsmPrinter;
class MCContext;
class MCSymbol;
class MachineInstr;
class MachineOperand;
class MCSymbolRefExpr;

/// This class is used to lower an MachineInstr into an MCInst.
class LLVM_LIBRARY_VISIBILITY JVMMCInstLower {
  MCContext &Ctx;
  JVMAsmPrinter &Printer;

public:
  JVMMCInstLower(MCContext &ctx, JVMAsmPrinter &printer)
      : Ctx(ctx), Printer(printer) {}
  void Lower(const MachineInstr *MI, MCInst &OutMI) const;

  const MCSymbolRefExpr *LowerSymbolOperand(const MachineOperand &MO) const;
};
} // end namespace llvm

#endif
