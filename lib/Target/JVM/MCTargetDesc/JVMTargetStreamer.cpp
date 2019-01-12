//==-- JVMTargetStreamer.cpp - JVM Target Streamer Methods --=//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
///
/// \file
/// \brief This file defines JVM-specific target streamer classes.
/// These are for implementing support for target-specific assembly directives.
///
//===----------------------------------------------------------------------===//

#include "JVMTargetStreamer.h"
#include "JVMMCTargetDesc.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/FormattedStream.h"

using namespace llvm;

JVMTargetStreamer::JVMTargetStreamer(MCStreamer &S) : MCTargetStreamer(S) {}

JVMTargetAsmStreamer::JVMTargetAsmStreamer(MCStreamer &S,
                                           formatted_raw_ostream &OS)
    : JVMTargetStreamer(S), OS(OS) {}

void JVMTargetAsmStreamer::emitString(const Twine &Str) { OS << Str << "\n"; }
