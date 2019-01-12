

message("   Moving jasmin.jar to <INSTALL_ROOT>/lib")
execute_process(
  COMMAND cd ${CMAKE_SOURCE_DIR}/tools/jasmin-2.4/
  COMMAND ./build.sh all
  COMMAND mv ${CMAKE_SOURCE_DIR}/tools/jasmin-2.4/jasmin.jar ${CMAKE_INSTALL_PREFIX}/lib/
) 

