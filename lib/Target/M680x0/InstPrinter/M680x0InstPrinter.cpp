//===-- M680x0InstPrinter.cpp - Convert M680x0 MCInst to asm ----*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
///
/// \file
/// This file contains definitions for an M680x0 MCInst printer.
///
//===----------------------------------------------------------------------===//

// TODO finish printer, it does not conform to Motorola asm at all

#include "M680x0InstPrinter.h"

#include "M680x0InstrInfo.h"

#include "llvm/ADT/StringExtras.h"
#include "llvm/MC/MCExpr.h"
#include "llvm/MC/MCInst.h"
#include "llvm/MC/MCInstrInfo.h"
#include "llvm/MC/MCSymbol.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/raw_ostream.h"

using namespace llvm;

#define DEBUG_TYPE "asm-printer"

#define PRINT_ALIAS_INSTR
#include "M680x0GenAsmWriter.inc"

void M680x0InstPrinter::printRegName(raw_ostream &OS, unsigned RegNo) const {
  OS << "%" << StringRef(getRegisterName(RegNo));
}

void M680x0InstPrinter::printInst(const MCInst *MI, raw_ostream &O,
                                  StringRef Annot, const MCSubtargetInfo &STI) {
  if (!printAliasInstr(MI, O)) {
    printInstruction(MI, O);
  }
  printAnnotation(O, Annot);
}

void M680x0InstPrinter::printOperand(const MCInst *MI, unsigned OpNo,
                                     raw_ostream &O) {
  const MCOperand &MO = MI->getOperand(OpNo);
  if (MO.isReg()) {
    printRegName(O, MO.getReg());
    return;
  }

  if (MO.isImm()) {
    printImmediate(MI, OpNo, O);
    return;
  }

  assert(MO.isExpr() && "Unknown operand kind in printOperand");
  MO.getExpr()->print(O, &MAI);
}

void M680x0InstPrinter::printImmediate(const MCInst *MI, int opNum,
                                       raw_ostream &O) {
  const MCOperand &MO = MI->getOperand(opNum);
  if (MO.isImm()) {
    O << '#' << MO.getImm();
  } else if (MO.isExpr()) {
    O << '#';
    MO.getExpr()->print(O, &MAI);
  } else {
    llvm_unreachable("Unknown immediate kind");
  }
}

//
void M680x0InstPrinter::printMoveMask(const MCInst *MI, int opNum,
                                      raw_ostream &O) {
  unsigned Mask = MI->getOperand(opNum).getImm();
  assert((Mask & 0xFFFF) == Mask);

  unsigned HalfMask, Reg;
  for (int s = 0; s < 8; s += 8) {
    HalfMask = Mask >> s;
    if (HalfMask && s != 0) {
      O << ',';
    }

    for (int i = 0; HalfMask; ++i) {
      if ((HalfMask >> i) & 0x01) {
        HalfMask ^= 1 << i;
        Reg = M680x0II::getMaskedSpillRegister(i + s);
        printRegName(O, Reg);

        int j = i;
        while ((HalfMask >> (j + 1)) & 0x01) {
          HalfMask ^= 1 << ++j;
        }

        if (j != i) {
          O << '-';
          Reg = M680x0II::getMaskedSpillRegister(j + s);
          printRegName(O, Reg);
        }

        i = j;

        if (HalfMask) {
          O << ',';
        }
      } else {
      }
    }
  }
}

void M680x0InstPrinter::printDisp(const MCInst *MI, int opNum, raw_ostream &O) {
  const MCOperand &Op = MI->getOperand(opNum);
  if (Op.isImm()) {
    O << Op.getImm();
    return;
  }
  assert(Op.isExpr() && "Unknown operand kind in printOperand");
  Op.getExpr()->print(O, &MAI);
}

void M680x0InstPrinter::printARIMem(const MCInst *MI, int opNum,
                                    raw_ostream &O) {
  O << '(';
  printOperand(MI, opNum, O);
  O << ')';
}

void M680x0InstPrinter::printARIPIMem(const MCInst *MI, int opNum,
                                      raw_ostream &O) {
  O << "(";
  printOperand(MI, opNum, O);
  O << ")+";
}

void M680x0InstPrinter::printARIPDMem(const MCInst *MI, int opNum,
                                      raw_ostream &O) {
  O << "-(";
  printOperand(MI, opNum, O);
  O << ")";
}

void M680x0InstPrinter::printARIDMem(const MCInst *MI, int opNum,
                                     raw_ostream &O) {
  O << '(';
  printDisp(MI, opNum + M680x0::MemDisp, O);
  O << ',';
  printOperand(MI, opNum + M680x0::MemBase, O);
  O << ')';
}

void M680x0InstPrinter::printARIIMem(const MCInst *MI, int opNum,
                                     raw_ostream &O) {
  O << '(';
  printDisp(MI, opNum + M680x0::MemDisp, O);
  O << ',';
  printOperand(MI, opNum + M680x0::MemBase, O);
  O << ',';
  printOperand(MI, opNum + M680x0::MemIndex, O);
  O << ')';
}

// NOTE forcing (W,L) size available since M68020 only
void M680x0InstPrinter::printAbsMem(const MCInst *MI, int opNum,
                                    raw_ostream &O) {
  const MCOperand &MO = MI->getOperand(opNum);
  if (MO.isImm()) {
    // ??? Print it in hex?
    O << (unsigned int)MO.getImm();
  } else {
    printOperand(MI, opNum, O);
  }
}

void M680x0InstPrinter::printPCDMem(const MCInst *MI, int opNum,
                                    raw_ostream &O) {
  O << '(';
  printDisp(MI, opNum + M680x0::PCRelDisp, O);
  O << ",%pc)";
}

void M680x0InstPrinter::printPCIMem(const MCInst *MI, int opNum,
                                    raw_ostream &O) {
  O << '(';
  printDisp(MI, opNum + M680x0::PCRelDisp, O);
  O << ",%pc,";
  printOperand(MI, opNum + M680x0::PCRelIndex, O);
  O << ')';
}
