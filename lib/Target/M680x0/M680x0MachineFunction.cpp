//===-- M680x0MachineFunctionInfo.cpp - M680x0 private data ----*- C++ -*--===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "M680x0MachineFunction.h"

#include "M680x0InstrInfo.h"
#include "M680x0Subtarget.h"

#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/MachineRegisterInfo.h"
#include "llvm/IR/Function.h"

using namespace llvm;

void M680x0MachineFunctionInfo::anchor() {}
