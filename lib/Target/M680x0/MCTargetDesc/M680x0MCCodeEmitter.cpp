//===-- M680x0MCCodeEmitter.cpp - Convert M680x0 code to machine code -----===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "MCTargetDesc/M680x0MCTargetDesc.h"
#include "MCTargetDesc/M680x0BaseInfo.h"
#include "MCTargetDesc/M680x0FixupKinds.h"
#include "M680x0RegisterInfo.h"
#include "llvm/MC/MCCodeEmitter.h"
#include "llvm/MC/MCContext.h"
#include "llvm/MC/MCExpr.h"
#include "llvm/MC/MCInst.h"
#include "llvm/MC/MCInstrInfo.h"
#include "llvm/MC/MCRegisterInfo.h"
#include "llvm/MC/MCSubtargetInfo.h"
#include "llvm/MC/MCSymbol.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/raw_ostream.h"

using namespace llvm;

#define DEBUG_TYPE "m680x0-mccodeemitter"

namespace {
class M680x0MCCodeEmitter : public MCCodeEmitter {
  M680x0MCCodeEmitter(const M680x0MCCodeEmitter &) = delete;
  void operator=(const M680x0MCCodeEmitter &) = delete;
  const MCInstrInfo &MCII;
  MCContext &Ctx;
public:
  M680x0MCCodeEmitter(const MCInstrInfo &mcii, MCContext &ctx)
    : MCII(mcii), Ctx(ctx) {
  }

  ~M680x0MCCodeEmitter() override {}

  // getGenInstrBeads - TableGen'erated function
  const uint8_t * getGenInstrBeads(const MCInst &MI) const;

  unsigned EncodeBits(unsigned ThisByte, uint8_t Bead, const MCInst &MI, const MCInstrDesc &Desc,
            uint64_t &Buffer, unsigned Offset, SmallVectorImpl<MCFixup> &Fixups,
            const MCSubtargetInfo &STI) const;

  unsigned EncodeReg(unsigned ThisByte, uint8_t Bead, const MCInst &MI, const MCInstrDesc &Desc,
            uint64_t &Buffer, unsigned Offset, SmallVectorImpl<MCFixup> &Fixups,
            const MCSubtargetInfo &STI) const;

  unsigned EncodeImm(unsigned ThisByte, uint8_t Bead, const MCInst &MI, const MCInstrDesc &Desc,
            uint64_t &Buffer, unsigned Offset, SmallVectorImpl<MCFixup> &Fixups,
            const MCSubtargetInfo &STI) const;

  void encodeInstruction(const MCInst &MI, raw_ostream &OS,
                         SmallVectorImpl<MCFixup> &Fixups,
                         const MCSubtargetInfo &STI) const override;
};

} // end anonymous namespace

unsigned M680x0MCCodeEmitter::
EncodeBits(unsigned ThisByte, uint8_t Bead, const MCInst &MI, const MCInstrDesc &Desc,
           uint64_t &Buffer, unsigned Offset, SmallVectorImpl<MCFixup> &Fixups,
           const MCSubtargetInfo &STI) const {
  unsigned Num = 0;
  switch (Bead & 0xF) {
    case M680x0Beads::Bits1: Num = 1; break;
    case M680x0Beads::Bits2: Num = 2; break;
    case M680x0Beads::Bits3: Num = 3; break;
    case M680x0Beads::Bits4: Num = 4; break;
  }
  unsigned char Val = (Bead & 0xF0) >> 4;

  DEBUG(dbgs() << "\tEncodeBits"
               << " Num: " << Num
               << " Val: 0x");
  DEBUG(dbgs().write_hex(Val) << "\n");

  Buffer |= (Val << Offset);

  return Num;
}

unsigned M680x0MCCodeEmitter::
EncodeReg(unsigned ThisByte, uint8_t Bead, const MCInst &MI, const MCInstrDesc &Desc,
          uint64_t &Buffer, unsigned Offset, SmallVectorImpl<MCFixup> &Fixups,
          const MCSubtargetInfo &STI) const {
  bool DA, Reg;
  switch (Bead & 0xF) {
    case M680x0Beads::DAReg: Reg = true;  DA = true; break;
    case M680x0Beads::DA:    Reg = false; DA = true; break;
    case M680x0Beads::Reg:   Reg = true;  DA = false; break;
  }

  unsigned Op = (Bead & 0x70) >> 4;
  bool Alt = (Bead & 0x80);
  DEBUG(dbgs() << "\tEncodeReg"
               << " Op: " << Op
               << ", DA: " << DA
               << ", Reg: " << Reg
               << ", Alt: " << Alt
               << "\n");

  assert (Op < Desc.NumMIOperands);
  MIOperandInfo MIO = Desc.MIOpInfo[Op];
  MCOperand MCO;
  // TODO PCRel operands are always Alt for reg
  if (MIO.isTargetType() && MIO.OpsNum > 1) {
    MCO = MI.getOperand(MIO.MINo + (Alt ? M680x0::MemIndex : M680x0::MemBase));
  } else {
    assert (!Alt && "You cannot use Alt register with a simple operand");
    MCO = MI.getOperand(MIO.MINo);
  }

  unsigned RegNum = MCO.getReg();
  auto RI = Ctx.getRegisterInfo();

  unsigned Written = 0;
  if (Reg) {
    uint32_t Val = RI->getEncodingValue(RegNum);
    Buffer |= Val << Offset;
    Offset+=3;
    Written+=3;
  }

  if (DA) {
    Buffer |= (char)M680x0II::isAddressRegister(RegNum) << Offset;
    Written++;
  }

  return Written;
}
/// intDoseFit - Checks if an integer fits into the given bit width.
/// non-templated version
LLVM_CONSTEXPR static inline bool intDoseFit(unsigned N, int64_t x) {
  return N >= 64 || (-(INT64_C(1)<<(N-1)) <= x && x < (INT64_C(1)<<(N-1)));
}

static unsigned
EmitConstant(int64_t Val, unsigned Size, unsigned Pad, uint64_t &Buffer, unsigned Offset) {
  assert (Size && (Size == 8 || Size == 16 || Size == 32));
  assert (Size + Offset <= 64 && "Value does not fit");
  assert (intDoseFit(Size, Val));

  // Pad the instruction with zeros if any
  Size += Pad;

  // Writing Value in host's endianness
  Buffer |= Val << Offset;
  return Size;
}

unsigned M680x0MCCodeEmitter::
EncodeImm(unsigned ThisByte, uint8_t Bead, const MCInst &MI, const MCInstrDesc &Desc,
          uint64_t &Buffer, unsigned Offset, SmallVectorImpl<MCFixup> &Fixups,
          const MCSubtargetInfo &STI) const {
  unsigned Size = 0;
  unsigned Pad = 0;
  unsigned FixOffset = 0;
  int64_t  ExprAdd = 0;
  switch (Bead & 0xF) {
    // Disp8 requires +1 byte offset since it is not padded and the target is BE
    // This will be fixed within the expression itself.
    // ??? what happens if it is not byte aligned
    // ??? is it even possible
    case M680x0Beads::Disp8:  Size = 8;  Pad = 0; FixOffset = ThisByte + 1; ExprAdd = -1; break;
    case M680x0Beads::Imm8:   Size = 8;  Pad = 8; FixOffset = ThisByte; break;
    case M680x0Beads::Imm16:  Size = 16; Pad = 0; FixOffset = ThisByte; break;
    case M680x0Beads::Imm32:  Size = 32; Pad = 0; FixOffset = ThisByte; break;
  }
  unsigned Op = (Bead & 0x70) >> 4;
  bool Alt = (Bead & 0x80);
  DEBUG(dbgs() << "\tEncodeImm"
               << " Op: " << Op
               << ", Size: " << Size
               << ", Alt: " << Alt
               << "\n");
  assert (Op < Desc.NumMIOperands);
  MIOperandInfo MIO = Desc.MIOpInfo[Op];
  MCOperand MCO;
  bool isPCRel = M680x0II::isPCRelOpd(MIO.Type);
  if (MIO.isTargetType()) {
    MCO = MI.getOperand(MIO.MINo + (Alt ? M680x0::MemOuter : M680x0::MemDisp));
    if (isPCRel) {
      assert(!Alt && "You cannot use ALT operand with PCRel");
      const MCExpr *Expr = nullptr;
      if (MCO.isImm()) {
        Expr = MCConstantExpr::create(MCO.getImm(), Ctx);
      } else {
        Expr = MCO.getExpr();
      }

      if (ExprAdd != 0) {
        Expr = MCBinaryExpr::createAdd(Expr,
                 MCConstantExpr::create(ExprAdd, Ctx), Ctx);
      }

      Fixups.push_back(
          MCFixup::create(FixOffset, Expr, getFixupForSize(Size, true), MI.getLoc()));
      // Write zeros
      return EmitConstant(0, Size, Pad, Buffer, Offset);
    }
  } else {
    assert (!Alt && "You cannot use Alt immediate with a simple operand");
    MCO = MI.getOperand(MIO.MINo);
    if (MCO.isExpr()) {
      const MCExpr *Expr = MCO.getExpr();

      if (ExprAdd != 0) {
        Expr = MCBinaryExpr::createAdd(Expr,
                 MCConstantExpr::create(ExprAdd, Ctx), Ctx);
      }

      Fixups.push_back(
          MCFixup::create(FixOffset, Expr, getFixupForSize(Size, isPCRel), MI.getLoc()));
      // Write zeros
      return EmitConstant(0, Size, Pad, Buffer, Offset);
    }
  }

  // 32 bit Imm requires HI16 first then LO16
  if (Size == 32) {
    uint64_t Imm = MCO.getImm();
    Offset += EmitConstant((Imm >> 16) & 0xFFFF, 16, Pad, Buffer, Offset);
    EmitConstant(Imm & 0xFFFF, 16, Pad, Buffer, Offset);
    return Size;
  }

  return EmitConstant(MCO.getImm(), Size, Pad, Buffer, Offset);
}

#include "M680x0GenMCCodeBeads.inc"

void M680x0MCCodeEmitter::
encodeInstruction(const MCInst &MI, raw_ostream &OS,
                  SmallVectorImpl<MCFixup> &Fixups,
                  const MCSubtargetInfo &STI) const {
  unsigned Opcode = MI.getOpcode();
  const MCInstrDesc &Desc = MCII.get(Opcode);
  // uint64_t TSFlags = Desc.TSFlags;

  DEBUG(dbgs() << "EncodeInstruction: "
      <<  MCII.getName(Opcode) << "(" << Opcode << ")\n");


  const uint8_t * Beads = getGenInstrBeads(MI);
  if (!*Beads) {
    llvm_unreachable("*** Instruction does not have Beads defined");
  }

  uint64_t Buffer = 0;
  unsigned Offset = 0;
  unsigned ThisByte = 0;

  while (*Beads) {
    uint8_t Bead = *Beads;
    Beads++;

    // Check for control beads
    if (!(Bead & 0xF)) {
      switch (Bead >> 4) {
        case M680x0Beads::Ignore:
          continue;
      }
    }

    switch (Bead & 0xF) {
      default: llvm_unreachable("Unknown Bead code");
        break;
      case M680x0Beads::Bits1:
      case M680x0Beads::Bits2:
      case M680x0Beads::Bits3:
      case M680x0Beads::Bits4:
        Offset += EncodeBits(ThisByte, Bead, MI, Desc, Buffer, Offset, Fixups, STI);
        break;
      case M680x0Beads::DAReg:
      case M680x0Beads::DA:
      case M680x0Beads::Reg:
        Offset += EncodeReg(ThisByte, Bead, MI, Desc, Buffer, Offset, Fixups, STI);
        break;
      case M680x0Beads::Disp8:
      case M680x0Beads::Imm8:
      case M680x0Beads::Imm16:
      case M680x0Beads::Imm32:
        Offset += EncodeImm(ThisByte, Bead, MI, Desc, Buffer, Offset, Fixups, STI);
        break;
    }

    // Since M680x0 is Big Endian we need to rotate each instruction word
    while (Offset / 16) {
      OS.write((char)((Buffer >> 8) & 0xFF));
      OS.write((char)((Buffer >> 0) & 0xFF));
      Buffer >>= 16;
      Offset -= 16;
      ThisByte += 2;
    }
  }

  // while (Offset >= 8) {
  //   OS.write((char)(Buffer & 0xFF));
  //   Buffer >>= 8;
  //   Offset -= 8;
  //   ThisByte++;
  // }

  assert (Offset == 0 && "M680x0 Instructions are % 2 bytes");
  assert ((ThisByte && !(ThisByte % 2)) && "M680x0 Instructions are % 2 bytes");
}

MCCodeEmitter *llvm::
createM680x0MCCodeEmitter(const MCInstrInfo &MCII,
                          const MCRegisterInfo &MRI,
                          MCContext &Ctx) {
  return new M680x0MCCodeEmitter(MCII, Ctx);
}
