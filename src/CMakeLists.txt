# Copyright (C) 2011 by the f2k3-lua authors, see AUTHORS file.
# Licensed under the MIT license, see LICENSE file.

include_directories(
  ${LUA_INCLUDE_DIR}
)

add_library(${PROJECT_NAME}
  clua.c
  flua.f90
  lua_c_interfaces.f90
)

target_link_libraries(f2k3_lua_simple
  ${LUA_LIBRARIES}
)

install(TARGETS ${PROJECT_NAME}
  EXPORT ${PROJECT_NAME}-targets
  ${DEFAULT_INSTALL_DESTINATIONS} 
)