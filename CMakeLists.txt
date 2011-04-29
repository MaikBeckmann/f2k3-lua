# Copyright (C) 2011 by the f2k3-lua authors, see AUTHORS file.
# Licensed under the MIT license, see LICENSE file.

cmake_minimum_required(VERSION 2.6)
project(f2k3_lua_simple C Fortran)

find_package(Lua51 REQUIRED)

include(details.cmake)
set(CMAKE_Fortran_MODULE_DIRECTORY "${PROJECT_BINARY_DIR}/include")

add_subdirectory(src)
add_subdirectory(test)

install(EXPORT ${PROJECT_NAME}-targets
  DESTINATION lib/${PROJECT_NAME}/cmake/
)
install(FILES ${PROJECT_NAME}-config.cmake
  DESTINATION lib/${PROJECT_NAME}/cmake/
)
install(DIRECTORY ${PROJECT_BINARY_DIR}/include/
  DESTINATION include/${PROJECT_NAME}
  FILES_MATCHING PATTERN "*.mod"
)

