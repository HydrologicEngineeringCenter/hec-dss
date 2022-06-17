project(heclib)
cmake_minimum_required(VERSION 3.16)


set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
enable_language(Fortran)

add_compile_options(-cpp)
file(GLOB_RECURSE F_SRC LIST_DIRECTORIES true heclib/heclib_f/src/*.f  heclib/heclib_f/src/*.f90)
file(GLOB_RECURSE C_SRC LIST_DIRECTORIES true heclib/heclib_c/src/*.c)
include_directories(heclib/heclib_c/src/headers)
include_directories(heclib/heclib_f/src/headers)
add_library(heclib STATIC ${F_SRC} ${C_SRC})