//=- JVMInstPrinter.cpp - JVM assembly instruction printing -=//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
///
/// \file
/// \brief Print MCInst instructions to jvm format.
///
//===----------------------------------------------------------------------===//

#include "InstPrinter/JVMInstPrinter.h"
#include "JVM.h"
#include "MCTargetDesc/JVMMCTargetDesc.h"
#include "llvm/ADT/SmallSet.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/CodeGen/TargetRegisterInfo.h"
#include "llvm/MC/MCExpr.h"
#include "llvm/MC/MCInst.h"
#include "llvm/MC/MCInstrInfo.h"
#include "llvm/MC/MCSubtargetInfo.h"
#include "llvm/MC/MCSymbol.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/FormattedStream.h"
using namespace llvm;

#define DEBUG_TYPE "asm-printer"

#include "JVMGenAsmWriter.inc"

JVMInstPrinter::JVMInstPrinter(const MCAsmInfo &MAI, const MCInstrInfo &MII,
                               const MCRegisterInfo &MRI)
    : MCInstPrinter(MAI, MII, MRI) {}

void JVMInstPrinter::printInst(const MCInst *MI, raw_ostream &OS,
                               StringRef Annot,
                               const MCSubtargetInfo & /*STI*/) {
  // Print the instruction (this uses the AsmStrings from the .td files).
  printInstruction(MI, OS);

  // Print any added annotation.
  printAnnotation(OS, Annot);
}

void JVMInstPrinter::printJVMDescriptorOperand(const MCInst *MI, unsigned OpNo,
                                               raw_ostream &O) {
  assert(DescVec && "Uninitialized descriptor vector");
  StringRef Desc = (*DescVec)[MI->getOperand(OpNo).getImm()];
  O << Desc;
}

void JVMInstPrinter::printOperand(const MCInst *MI, unsigned OpNo,
                                  raw_ostream &O) {
  const MCOperand &Op = MI->getOperand(OpNo);
  const MCInstrDesc &Desc = MII.get(MI->getOpcode());
  const MCOperandInfo &Info = Desc.OpInfo[OpNo];
  if (Op.isReg()) {
    llvm_unreachable("Unexpected register operand.");
  } else if (Op.isImm()) {
    O << Op.getImm();
  } else if (Op.isFPImm()) {
    assert(OpNo < Desc.getNumOperands() &&
           "Unexpected floating-point immediate as a non-fixed operand");
    assert(Desc.TSFlags == 0 &&
           "JVM variable_ops floating point ops don't use TSFlags");
    if (Info.OperandType == JVM::OPERAND_F32IMM) {
      // TODO: MC converts all floating point immediate operands to double.
      // This is fine for numeric values, but may cause NaNs to change bits.
      O << Op.getFPImm();
    } else {
      assert(Info.OperandType == JVM::OPERAND_F64IMM);
      O << Op.getFPImm();
    }
  } else {
    assert(Op.isExpr() && "unknown operand kind in printOperand");
    Op.getExpr()->print(O, &MAI);
  }
}
