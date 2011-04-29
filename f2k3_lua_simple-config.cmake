get_filename_component(SELF_DIR "${CMAKE_CURRENT_LIST_FILE}" PATH)
include(${SELF_DIR}/f2k3_lua_simple-targets.cmake)
get_filename_component(f2k3_lua_simple_INCLUDE_DIR 
  "${SELF_DIR}/../../../include/f2k3_lua_simple" ABSOLUTE)
set(f2k3_lua_simple_INCLUDE_DIRS ${f2k3_lua_simple_INCLUDE_DIR})

set(f2k3_lua_simple_LIBRARIES f2k3_lua_simple)