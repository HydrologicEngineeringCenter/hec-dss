

add_library(hecdss SHARED hecdss.c hecdss.h)

if( ${CMAKE_C_COMPILER_ID} MATCHES GNU)    
    target_compile_options(hecdss PRIVATE -fPIC -std=gnu99)
elseif(${CMAKE_C_COMPILER_ID} MATCHES SunPro)
    target_compiler_options(hecdss PRIVATE -m64 -xmemalign=2i -xcode=pic32 -c -Isrc/headers -mt -xcode=abs64 -xstrconst -xc99=all -xCC -g -xs)
elseif(${CMAKE_C_COMPILER_ID} MATCHES MSVC)
    set(HECDSS_EXPORTS true)
else()
    message(STATUS "generic options")
endif()

target_link_libraries(hecdss heclib)
