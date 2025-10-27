

add_library(hecdss SHARED)

if( ${CMAKE_C_COMPILER_ID} MATCHES GNU)    
    target_compile_options(hecdss PRIVATE -fPIC -std=gnu99)
elseif(${CMAKE_C_COMPILER_ID} MATCHES SunPro)
    target_compiler_options(hecdss PRIVATE -m64 -xmemalign=2i -xcode=pic32 -c -Isrc/headers -mt -xcode=abs64 -xstrconst -xc99=all -xCC -g -xs)
elseif(MSVC)
    target_compile_definitions(hecdss PRIVATE HECDSS_EXPORTS)
else()
    message(STATUS "hecdss library will use generic options")
endif()

target_link_libraries(hecdss heclib)
target_sources(hecdss PRIVATE hecdss.c)
target_sources(hecdss PUBLIC hecdss.h)
