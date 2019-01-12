
set(LLVM_TARGET_DEFINITIONS JVM.td)

tablegen(LLVM JVMGenAsmWriter.inc -gen-asm-writer)
tablegen(LLVM JVMGenDAGISel.inc -gen-dag-isel)
tablegen(LLVM JVMGenInstrInfo.inc -gen-instr-info)
tablegen(LLVM JVMGenRegisterInfo.inc -gen-register-info)
tablegen(LLVM JVMGenSubtargetInfo.inc -gen-subtarget)
add_public_tablegen_target(JVMCommonTableGen)


add_llvm_target(JVMCodeGen
  JVMAsmPrinter.cpp
  JVMFrameLowering.cpp
  JVMISelDAGToDAG.cpp
  JVMISelLowering.cpp
  JVMInstrInfo.cpp
  JVMMachineFunctionInfo.cpp
  JVMMCInstLower.cpp
  JVMMICommonUtils.cpp
  JVMRegisterInfo.cpp
  JVMSelectionDAGInfo.cpp
  JVMSubtarget.cpp
  JVMTargetMachine.cpp
		JVMPreAllocationFixups.cpp
  JVMTargetObjectFile.cpp
  JVMLoadStoreEliminationOpt.cpp
  JVMOffsetAllocator.cpp
  JVMPostAllocationFixups.cpp
  JVMCopyElision.cpp

  DEPENDS
  intrinsics_gen
)

add_subdirectory(Disassembler)
add_subdirectory(InstPrinter)
add_subdirectory(MCTargetDesc)
add_subdirectory(TargetInfo)
add_subdirectory(AssemblerNativeUtils)
add_subdirectory(Utils)
