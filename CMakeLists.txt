cmake_minimum_required(VERSION 2.6)
project(f2k3-lua C Fortran)

find_package(Lua51 REQUIRED)

add_subdirectory(src)
add_subdirectory(test)