//===-- M680x0ELFTargetObjectFile.cpp - M680x0 Object Files -----*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
///
/// \file
/// This file contains definitions for M680x0 ELF object file lowering.
///
//===----------------------------------------------------------------------===//

#include "M680x0TargetObjectFile.h"

#include "M680x0Subtarget.h"
#include "M680x0TargetMachine.h"

#include "llvm/BinaryFormat/ELF.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/GlobalVariable.h"
#include "llvm/MC/MCContext.h"
#include "llvm/MC/MCSectionELF.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Target/TargetMachine.h"

using namespace llvm;

static cl::opt<unsigned> SSThreshold(
    "m680x0-ssection-threshold", cl::Hidden,
    cl::desc("Small data and bss section threshold size (default=8)"),
    cl::init(8));

void M680x0ELFTargetObjectFile::Initialize(MCContext &Ctx,
                                           const TargetMachine &TM) {
  TargetLoweringObjectFileELF::Initialize(Ctx, TM);
  InitializeELF(TM.Options.UseInitArray);

  this->TM = &static_cast<const M680x0TargetMachine &>(TM);

  // FIXME #32 do i need them explicitly?
  SmallDataSection = getContext().getELFSection(
      ".sdata", ELF::SHT_PROGBITS, ELF::SHF_WRITE | ELF::SHF_ALLOC);

  SmallBSSSection = getContext().getELFSection(".sbss", ELF::SHT_NOBITS,
                                               ELF::SHF_WRITE | ELF::SHF_ALLOC);
}
