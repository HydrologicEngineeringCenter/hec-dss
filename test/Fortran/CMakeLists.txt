file(GLOB F_SRC LIST_DIRECTORIES true source/*.f90)
add_executable(Dss-Fortran  ${F_SRC})
target_link_libraries(Dss-Fortran heclib)
target_include_directories(Dss-Fortran PRIVATE ${CMAKE_CURRENT_SOURCE_DIR})
if( NOT CMAKE_HOST_WIN32)
	target_compile_options(Dss-Fortran PRIVATE -cpp -std=gnu -Werror)   
elseif(MSVC)
    
    set_property(TARGET Dss-Fortran PROPERTY VS_SOURCE_SETTINGS_VFFortranCompilerTool 
        "CallingConvention=callConventionCRef"
        "ExternalNameUnderscore=true" 
        "Traceback=true" "RuntimeChecks=rtChecksAll"
        "BoundsCheck=true" 
        "RuntimeLibrary=rtMultiThreadedDebug" 
        "UsePortlib=true"
    
    )
    
    set_property(TARGET Dss-Fortran PROPERTY LINKER_LANGUAGE Fortran)
    target_compile_options(Dss-Fortran PRIVATE /nologo  /MP -Od -fpp  -reentrancy:threaded -warn:noalignments 
                                               /Qsave /Qinit:zero /names:lowercase /iface:cref -assume:underscore  /traceback 
                                               /check:all /libs:static $(DLIB) /threads -4Yportlib  )
    target_link_options(Dss-Fortran PRIVATE  /nologo /MP /names:lowercase /iface:cref -assume:underscore  /traceback 
                                             /check:all /libs:static $(DLIB) /threads -4Yportlib  )
endif()

add_custom_target(
    copy-test-data-dss-fortran ALL
    COMMAND ${CMAKE_COMMAND} -E copy_directory "${CMAKE_SOURCE_DIR}/dss-test-data" "${CMAKE_CURRENT_BINARY_DIR}/test"
    DEPENDS ${PROJECT_SOURCE_DIR}/dss-test-data
)


add_test(NAME Dss-Fortran COMMAND Dss-Fortran test WORKING_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/test")
