
include(FindJNI)
include_directories(${JAVA_INCLUDE_PATH} ${JAVA_INCLUDE_PATH2})

add_llvm_library(LLVMJVMNativeUtils
  JVMAssemblerNativeUtils.cpp 
)

