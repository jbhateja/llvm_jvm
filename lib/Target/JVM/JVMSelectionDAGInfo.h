//=- JVMSelectionDAGInfo.h - JVM SelectionDAG Info -*- C++ -*-//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
///
/// \file
/// \brief This file defines the JVM subclass for
/// SelectionDAGTargetInfo.
///
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_JVM_JVMSELECTIONDAGINFO_H
#define LLVM_LIB_TARGET_JVM_JVMSELECTIONDAGINFO_H

#include "llvm/CodeGen/SelectionDAGTargetInfo.h"

namespace llvm {

class JVMSelectionDAGInfo final : public SelectionDAGTargetInfo {
public:
  ~JVMSelectionDAGInfo() override;
};
} // end namespace llvm

#endif
