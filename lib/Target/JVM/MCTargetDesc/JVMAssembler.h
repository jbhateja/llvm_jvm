//===-- JVMAssembler.h - Top-level interface for JVMAssembler ---*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_JVM_ASSEMBLER_H
#define LLVM_LIB_TARGET_JVM_ASSEMBLER_H

#include "llvm/ADT/Optional.h"
#include <memory>
#include <string>

namespace llvm {

class JVMAssemblerImpl;

class JVMAssembler {
public:
  class JVMClassPacket {
  public:
    JVMClassPacket(char *packet);

    JVMClassPacket(const JVMClassPacket &) = delete;
    JVMClassPacket &operator=(const JVMClassPacket &) = delete;

    JVMClassPacket(const JVMClassPacket &&obj);
    JVMClassPacket &&operator=(const JVMClassPacket &&obj);

    void dump();
    int getSize() { return size; }
    char *getPayload() { return payload; }

  private:
    int size;
    char *payload;
  };

  JVMAssembler();
  JVMAssembler(const JVMAssembler &) = delete;
  JVMAssembler &operator=(const JVMAssembler &) = delete;
  ~JVMAssembler();

  Optional<std::unique_ptr<JVMClassPacket>> AssembleClass(std::string asmFile);

private:
  JVMAssemblerImpl *Impl;
};
} // end namespace llvm

#endif
