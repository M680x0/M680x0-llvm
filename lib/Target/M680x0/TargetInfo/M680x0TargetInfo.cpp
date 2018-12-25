//===-- M680x0TargetInfo.cpp - M680x0 Target Implementation -----*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
///
/// \file
/// This file contains M680x0 target initializer.
///
//===----------------------------------------------------------------------===//

#include "M680x0.h"

#include "MCTargetDesc/M680x0MCTargetDesc.h"

#include "llvm/Support/TargetRegistry.h"

using namespace llvm;

Target llvm::TheM680x0Target;

extern "C" void LLVMInitializeM680x0TargetInfo() {
  RegisterTarget<Triple::m680x0, /*HasJIT=*/true> X(
      TheM680x0Target, "m680x0", "Motorola 68000 family", "M680x0");
}
