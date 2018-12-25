//===---------- M680x0ELFObjectWriter.cpp - M680x0 ELF Writer ---*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
///
/// \file
/// This file contains definitions for M680x0 ELF Writers
///
//===----------------------------------------------------------------------===//

#include "MCTargetDesc/M680x0FixupKinds.h"
#include "MCTargetDesc/M680x0MCTargetDesc.h"

#include "llvm/BinaryFormat/ELF.h"
#include "llvm/MC/MCAsmInfo.h"
#include "llvm/MC/MCContext.h"
#include "llvm/MC/MCELFObjectWriter.h"
#include "llvm/MC/MCExpr.h"
#include "llvm/MC/MCValue.h"
#include "llvm/Support/ErrorHandling.h"

using namespace llvm;

namespace {
class M680x0ELFObjectWriter : public MCELFObjectTargetWriter {
public:
  M680x0ELFObjectWriter(uint8_t OSABI);

  ~M680x0ELFObjectWriter() override;

protected:
  unsigned getRelocType(MCContext &Ctx, const MCValue &Target,
                        const MCFixup &Fixup, bool IsPCRel) const override;
};
} // namespace

M680x0ELFObjectWriter::M680x0ELFObjectWriter(uint8_t OSABI)
    : MCELFObjectTargetWriter(false, OSABI, ELF::EM_68K, /* RELA */ true) {}

M680x0ELFObjectWriter::~M680x0ELFObjectWriter() {}

enum M680x0RelType { RT_32, RT_16, RT_8 };

static M680x0RelType
getType(unsigned Kind, MCSymbolRefExpr::VariantKind &Modifier, bool &IsPCRel) {
  switch (Kind) {
  default:
    llvm_unreachable("Unimplemented");
  case FK_Data_4:
  case FK_PCRel_4:
    return RT_32;
  case FK_PCRel_2:
  case FK_Data_2:
    return RT_16;
  case FK_PCRel_1:
  case FK_Data_1:
    return RT_8;
  }
}

// FIXME Should i split reloc types between pre x20 and the rest?
unsigned M680x0ELFObjectWriter::getRelocType(MCContext &Ctx,
                                             const MCValue &Target,
                                             const MCFixup &Fixup,
                                             bool IsPCRel) const {
  MCSymbolRefExpr::VariantKind Modifier = Target.getAccessVariant();
  unsigned Kind = Fixup.getKind();
  M680x0RelType Type = getType(Kind, Modifier, IsPCRel);
  switch (Modifier) {
  default:
    llvm_unreachable("Unimplemented");
  case MCSymbolRefExpr::VK_None:
    switch (Type) {
    case RT_32:
      return IsPCRel ? ELF::R_M680x0_PC32 : ELF::R_M680x0_32;
    case RT_16:
      return IsPCRel ? ELF::R_M680x0_PC16 : ELF::R_M680x0_16;
    case RT_8:
      return IsPCRel ? ELF::R_M680x0_PC8 : ELF::R_M680x0_8;
    }
  // case MCSymbolRefExpr::VK_GOT:
  //   switch (Type) {
  //   case RT_32:
  //     return IsPCRel ? ELF::R_M680x0_GOTPC32 : ELF::R_M680x0_GOT32;
  //   case RT_16:
  //     return IsPCRel ? ELF::R_M680x0_GOTPC16 : ELF::R_M680x0_GOT16;
  //   case RT_8:
  //     llvm_unreachable("Unimplemented");
  //   }
  case MCSymbolRefExpr::VK_GOTPCREL:
    switch (Type) {
    case RT_32:
      return ELF::R_M680x0_GOTPCREL32;
    case RT_16:
      return ELF::R_M680x0_GOTPCREL16;
    case RT_8:
      return ELF::R_M680x0_GOTPCREL8;
    }
  case MCSymbolRefExpr::VK_GOTOFF:
    assert(!IsPCRel);
    switch (Type) {
    case RT_32:
      return ELF::R_M680x0_GOTOFF32;
    case RT_16:
      return ELF::R_M680x0_GOTOFF16;
    case RT_8:
      return ELF::R_M680x0_GOTOFF8;
    }
  case MCSymbolRefExpr::VK_PLT:
    switch (Type) {
    case RT_32:
      return ELF::R_M680x0_PLT32;
    case RT_16:
      return ELF::R_M680x0_PLT16;
    case RT_8:
      return ELF::R_M680x0_PLT8;
    }
  }
}

std::unique_ptr<MCObjectTargetWriter>
llvm::createM680x0ELFObjectWriter(uint8_t OSABI) {
  return llvm::make_unique<M680x0ELFObjectWriter>(OSABI);
}
