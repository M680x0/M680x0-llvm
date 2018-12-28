//===-- M680x0InstrInfo.h - M680x0 Instruction Information ------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
///
/// \file
/// FinalAction allows you to ensure something gets run at the end of a scope
/// Copied from https://github.com/Microsoft/GSL
///
//===----------------------------------------------------------------------===//
#ifndef FINALACTION_H_OWGIVZ0T
#define FINALACTION_H_OWGIVZ0T

#include <array>
#include <cstddef>          // for ptrdiff_t, size_t
#include <exception>        // for exception
#include <initializer_list> // for initializer_list
#include <type_traits>      // for is_signed, integral_constant
#include <utility>          // for forward

namespace llvm {

// FinalAction allows you to ensure something gets run at the end of a scope
template <class F> class FinalAction {
public:
  explicit FinalAction(F f) noexcept : func(std::move(f)) {}

  FinalAction(FinalAction &&other) noexcept
      : func(std::move(other.func)), Invoke(other.Invoke) {
    other.Invoke = false;
  }

  FinalAction(const FinalAction &) = delete;
  FinalAction &operator=(const FinalAction &) = delete;
  FinalAction &operator=(FinalAction &&) = delete;

  ~FinalAction() noexcept {
    if (Invoke)
      func();
  }

private:
  F func;
  bool Invoke{true};
};

// finally() - convenience function to generate a FinalAction
template <class F> FinalAction<F> finally(const F &f) noexcept {
  return FinalAction<F>(f);
}

template <class F> FinalAction<F> finally(F &&f) noexcept {
  return FinalAction<F>(std::forward<F>(f));
}

} // namespace llvm

#endif // FINALACTION_H_OWGIVZ0T
