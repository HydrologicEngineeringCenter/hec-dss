file(GLOB_RECURSE F_SRC LIST_DIRECTORIES true src/*.f  src/*.f90)


add_library(heclib_f OBJECT  ${F_SRC})
target_include_directories(heclib_f PUBLIC src/headers)
target_link_libraries(heclib_f PRIVATE ZLIB::ZLIB)
if( ${CMAKE_Fortran_COMPILER_ID} MATCHES GNU)
    target_compile_options(heclib_f PRIVATE -cpp -fPIC -fallow-argument-mismatch -Warray-bounds -Wc-binding-type -Wcharacter-truncation)
elseif( ${CMAKE_Fortran_COMPILER_ID} MATCHES SunPro)
    target_compile_options(heclib_f -m64 -c -fpp -mt -xmemalign=2i -xcode=pic32 -xpagesize=4M -Isrc/headers -O)
elseif(MSVC)
    target_compile_options(heclib_f PRIVATE /nologo /debug:$(D_LEVEL) /MP /Od /fpp  /reentrancy:threaded /warn:noalignments 
 /Qsave /Qinit:zero /names:lowercase /iface:cref /assume:underscore  /traceback 
  /check:all /libs:static $(DLIB) /threads /4Yportlib /c  )
endif()
