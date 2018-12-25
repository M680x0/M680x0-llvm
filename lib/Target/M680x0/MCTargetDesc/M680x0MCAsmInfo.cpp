//===-- M680x0MCAsmInfo.cpp - M680x0 Asm Properties -------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
///
/// \file
/// This file contains the definitions of the M680x0 MCAsmInfo properties.
///
//===----------------------------------------------------------------------===//

#include "M680x0MCAsmInfo.h"

#include "llvm/ADT/Triple.h"

using namespace llvm;

void M680x0ELFMCAsmInfo::anchor() {}

// TODO get back to it when it comes to printing
M680x0ELFMCAsmInfo::M680x0ELFMCAsmInfo(const Triple &T) {
  CodePointerSize = 4;
  CalleeSaveStackSlotSize = 4;

  IsLittleEndian = false;

  TextAlignFillValue = 0x90;

  // Debug Information
  SupportsDebugInformation = true;

  // Exceptions handling
  ExceptionsType = ExceptionHandling::DwarfCFI;

  // Always enable the integrated assembler by default.
  // Clang also enabled it when the OS is Solaris but that is redundant here.
  UseIntegratedAssembler = true;

  CommentString = ";";
}
