//===-------JVMMCInstLower.cpp - Convert JVM MachineInstr to an MCInst-----===
////
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
///
/// \file
/// \brief This file contains code to lower JVM MachineInstrs to their
/// corresponding MCInst records.
///
//===----------------------------------------------------------------------===//

#include "JVMMCInstLower.h"
#include "JVMAsmPrinter.h"
#include "JVMMachineFunctionInfo.h"
#include "llvm/CodeGen/AsmPrinter.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/IR/Constants.h"
#include "llvm/MC/MCAsmInfo.h"
#include "llvm/MC/MCContext.h"
#include "llvm/MC/MCExpr.h"
#include "llvm/MC/MCInst.h"
#include "llvm/MC/MCSymbolELF.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/raw_ostream.h"
using namespace llvm;

const MCSymbolRefExpr *
JVMMCInstLower::LowerSymbolOperand(const MachineOperand &MO) const {
  assert(MO.isMBB() && "Incorrect machine operand passed for MC lowering.");
  const MCSymbol *Symbol = MO.getMBB()->getSymbol();
  return MCSymbolRefExpr::create(Symbol, Ctx);
}

void JVMMCInstLower::Lower(const MachineInstr *MI, MCInst &OutMI) const {
  OutMI.setOpcode(MI->getOpcode());
  const MachineFunction *MF = MI->getParent()->getParent();
  const MachineRegisterInfo *MRI = &MF->getRegInfo();
  for (unsigned i = 0, e = MI->getNumOperands(); i != e; ++i) {
    const MachineOperand &MO = MI->getOperand(i);
    MCOperand MCOp;
    switch (MO.getType()) {
    default:
      MI->print(errs());
      llvm_unreachable("unknown operand type");
    case MachineOperand::MO_MachineBasicBlock: {
      const MCSymbolRefExpr *Expr = LowerSymbolOperand(MO);
      MCOp = MCOperand::createExpr(Expr);
      break;
    }
    case MachineOperand::MO_Register: {
      int LocalOffset;
      JVMFunctionInfo *MFI = const_cast<JVMFunctionInfo *>(
          MI->getParent()->getParent()->getInfo<JVMFunctionInfo>());
      if (MRI->hasOneDef(MO.getReg()) &&
          MRI->getVRegDef(MO.getReg())->skipAllocation())
        LocalOffset = -1;
      else
        LocalOffset = MFI->getRegisterOffset(MO.getReg());
      MCOp = MCOperand::createImm(LocalOffset);
      break;
    }
    case MachineOperand::MO_Immediate:
      MCOp = MCOperand::createImm(MO.getImm());
      break;
    case MachineOperand::MO_FPImmediate: {
      // TODO: MC converts all floating point immediate operands to double.
      // This is fine for numeric values, but may cause NaNs to change bits.
      const ConstantFP *Imm = MO.getFPImm();
      if (Imm->getType()->isFloatTy())
        MCOp = MCOperand::createFPImm(Imm->getValueAPF().convertToFloat());
      else if (Imm->getType()->isDoubleTy())
        MCOp = MCOperand::createFPImm(Imm->getValueAPF().convertToDouble());
      else
        llvm_unreachable("unknown floating point immediate type");
      break;
    }
    case MachineOperand::MO_ExternalSymbol:
      assert("JVM does not support External Symbols operands.");
      break;
    case MachineOperand::MO_GlobalAddress: {
      const GlobalValue *GV = MO.getGlobal();
      assert(GV->getName() == "ConstantInitStruct");
      MCOp = MCOperand::createImm(0);
      break;
    }
    }
    OutMI.addOperand(MCOp);
  }
}
