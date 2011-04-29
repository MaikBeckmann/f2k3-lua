# Copyright (C) 2011 by the f2k3-lua authors, see AUTHORS file.
# Licensed under the MIT license, see LICENSE file.

include_directories(
  ${PROJECT_BINARY_DIR}/src
)
add_executable(test test.f90)
target_link_libraries(test
  f2k3lua  
)