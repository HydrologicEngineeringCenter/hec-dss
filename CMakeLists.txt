cmake_minimum_required(VERSION 3.16)
project(heclib LANGUAGES C)

# Statically link the MSVC runtime so the shipped DLL needs no VC++ redistributable
# (VCRUNTIME140.dll); CMP0091 (NEW under the 3.16 minimum) applies it to every target.
if(MSVC)
    set(CMAKE_MSVC_RUNTIME_LIBRARY "MultiThreaded$<$<CONFIG:Debug>:Debug>")
endif()

function(heclib_set_compile_options target)
    if(${CMAKE_C_COMPILER_ID} MATCHES GNU)
        target_compile_options(${target} PRIVATE -fPIC -std=gnu99 -Wall)
    elseif(${CMAKE_C_COMPILER_ID} MATCHES SunPro)
        target_compile_options(${target} PRIVATE -m64 -xmemalign=2i -xcode=pic32 -c -Isrc/headers -mt -xcode=abs64 -xstrconst -xc99=all -xCC -g -xs)
    elseif(MSVC)
        target_compile_options(${target} PRIVATE /W4 /MP)
    else()
        message(STATUS "generic options for ${target}")
    endif()
endfunction()

find_package(JNI QUIET)
add_subdirectory(deps)
add_subdirectory(heclib)

find_package(Git QUIET)

# Testing is always enabled so this check runs on every build, even without the optional
# DSS test-data repo -- it needs only the built lib:
#   self_contained -- the shipped native links nothing outside the OS baseline, so a
#   regression to a shared zlib / VC++ runtime, or the macOS @rpath load failure, fails
#   the build.
enable_testing()
add_test(
    NAME self_contained
    COMMAND ${CMAKE_COMMAND} -DLIB=$<TARGET_FILE:hecdss>
            -P ${CMAKE_SOURCE_DIR}/cmake/verify_self_contained.cmake
)
if(DSS_TEST_DATA_DIR)
    option(RUN_TESTS "Run functional tests, requires dss-test-data git repo." ON)
    if(RUN_TESTS)
        add_subdirectory(test)
    endif()
else()
    message(STATUS "Dss Test Data could not be retrieved -- functional tests skipped.")
endif()
