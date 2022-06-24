file(GLOB_RECURSE F_SRC LIST_DIRECTORIES true src/*.f  src/*.f90)


add_library(heclib_f OBJECT  ${F_SRC})
target_include_directories(heclib_f PUBLIC src/headers)
target_link_libraries(heclib_f PRIVATE ZLIB::ZLIB)
target_compile_options(heclib_f PRIVATE -fPIC )
if( NOT CMAKE_HOST_WIN32)
    target_compile_options(heclib_f PRIVATE -cpp)
    
else()
    target_compile_options(heclib_f PRIVATE /nologo /debug:$(D_LEVEL) /MP /Od /fpp  /reentrancy:threaded /warn:noalignments 
 /Qsave /Qinit:zero /names:lowercase /iface:cref /assume:underscore  /traceback 
  /check:all /libs:static $(DLIB) /threads /4Yportlib /c  )
endif()
