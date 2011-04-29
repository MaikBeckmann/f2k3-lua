# Copyright (C) 2011 by the f2k3-lua authors, see AUTHORS file.
# Licensed under the MIT license, see LICENSE file.

cmake_minimum_required(VERSION 2.6)
project(f2k3-lua C Fortran)

find_package(Lua51 REQUIRED)

add_subdirectory(src)
add_subdirectory(test)

# install(EXPORT ${PROJECT_NAME}-targets
#   DESTINATION share/cmake/${PROJECT_NAME}
# )
# install(FILES ${PROJECT_NAME}-config.cmake
#   DESTINATION share/cmake/${PROJECT_NAME}
# )
