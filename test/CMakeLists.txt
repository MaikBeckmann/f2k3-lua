include_directories(
  ${PROJECT_BINARY_DIR}/src
)
add_executable(test test.f90)
target_link_libraries(test
  f2k3lua  
)