file(GLOB_RECURSE C_SRC LIST_DIRECTORIES true src/*.c)

add_library(heclib_c OBJECT ${C_SRC})

if( ${CMAKE_C_COMPILER_ID} MATCHES GNU)    
    target_compile_options(heclib_c PRIVATE -fPIC -std=gnu99 -fcommon)
elseif(${CMAKE_C_COMPILER_ID} MATCHES SunPro)
    target_compiler_options(heclib_c PRIVATE -m64 -xmemalign=2i -xcode=pic32 -c -Isrc/headers -mt -xcode=abs64 -xstrconst -xc99=all -xCC -g -xs)
else()
    message(STATUS "generic options")
endif()
target_include_directories(heclib_c PUBLIC src/headers)
target_link_libraries(heclib_c PRIVATE ZLIB::ZLIB)
