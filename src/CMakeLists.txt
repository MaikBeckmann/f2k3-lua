include_directories(
  ${LUA_INCLUDE_DIR}
)

add_library(f2k3lua
  clua.c
  flua.f90
  lua_c_interfaces.f90
)

target_link_libraries(f2k3lua
  ${LUA_LIBRARIES}
)
