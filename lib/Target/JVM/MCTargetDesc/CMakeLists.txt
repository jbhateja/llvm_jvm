
include(FindJNI)
message("   JAVA_INCLUDE_PATH = " ${JAVA_INCLUDE_PATH})
message("   JAVA_INCLUDE_PATH2 = " ${JAVA_INCLUDE_PATH2})
include_directories(${JAVA_INCLUDE_PATH} ${JAVA_INCLUDE_PATH2})

add_llvm_library(LLVMJVMDesc
  JVMAsmBackend.cpp
  JVMAssembler.cpp
  JVMMCAsmInfo.cpp
  JVMMCTargetDesc.cpp
  JVMTargetStreamer.cpp
)

message("   Creating a softlink for libjvm.so")
execute_process(COMMAND ln -s ${JAVA_JVM_LIBRARY} ${CMAKE_INSTALL_PREFIX}/lib/)
