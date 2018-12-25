//===- M680x0.h - Top-level interface for M680x0 representation -*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
///
/// \file
/// This file contains the entry points for global functions defined in the
/// M680x0 target library, as used by the LLVM JIT.
///
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_M680x0_M680x0_H
#define LLVM_LIB_TARGET_M680x0_M680x0_H

#include "llvm/Support/CodeGen.h"

namespace llvm {

class FunctionPass;
class M680x0TargetMachine;

/// This pass converts a legalized DAG into a M680x0-specific DAG, ready for
/// instruction scheduling.
FunctionPass *createM680x0ISelDag(M680x0TargetMachine &TM);

/// Return a Machine IR pass that expands M680x0-specific pseudo
/// instructions into a sequence of actual instructions. This pass
/// must run after prologue/epilogue insertion and before lowering
/// the MachineInstr to MC.
FunctionPass *createM680x0ExpandPseudoPass();

/// This pass initializes a global base register for PIC on M680x0.
FunctionPass *createM680x0GlobalBaseRegPass();

/// Finds sequential MOVEM instruction and collapse them into a single one. This
/// pass has to be run after all pseudo expansions and prologue/epilogue
/// emission so that all possible MOVEM are already in place.
FunctionPass *createM680x0CollapseMOVEMPass();

/// Finds MOVE instructions before any conditioanl branch instruction and
/// replaces them with MOVEM instruction. Motorola's MOVEs do trash(V,C) flags
/// register which prevents branch from taking the correct route. This pass
/// has to be run after all pseudo expansions and prologue/epilogue emission
/// so that all possible MOVEs are present.
FunctionPass *createM680x0ConvertMOVToMOVMPass();

} // namespace llvm

#endif
