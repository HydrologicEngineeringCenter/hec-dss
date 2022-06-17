project(heclib)
cmake_minimum_required(VERSION 3.16)


set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
enable_language(Fortran)

if( NOT CMAKE_HOST_WIN32)
    add_compile_options(-cpp)
endif()

add_subdirectory(deps)
add_subdirectory(heclib)