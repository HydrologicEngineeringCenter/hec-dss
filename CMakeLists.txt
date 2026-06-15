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
find_package(Java COMPONENTS Development QUIET)  # javac/java for the JNI load test
add_subdirectory(deps)
add_subdirectory(heclib)

find_package(Git QUIET)

# hec_dss_api_version() in hecdss.c is the single source of truth for the version --
# parse it once here (used by both the consumer smoke test and the package name below)
# rather than restate it, so neither can drift from the API version.
file(STRINGS heclib/hecdss/hecdss.c _ver_line REGEX "return[ ]*\"[0-9]+\\.[0-9]+\\.[0-9]+\"")
string(REGEX MATCH "[0-9]+\\.[0-9]+\\.[0-9]+" HECDSS_VERSION "${_ver_line}")
if(NOT HECDSS_VERSION)
    message(FATAL_ERROR "Could not parse version from hecdss.c hec_dss_api_version()")
endif()

# Testing is always enabled so these checks run on every build, even without the
# optional DSS test-data repo -- they need only the built lib:
#   self_contained -- the shipped native links nothing outside the OS baseline.
#   consume_hecdss -- the shipped native is usable through its public C API/ABI.
enable_testing()
add_test(
    NAME self_contained
    COMMAND ${CMAKE_COMMAND} -DLIB=$<TARGET_FILE:hecdss>
            -P ${CMAKE_SOURCE_DIR}/cmake/verify_self_contained.cmake
)
add_subdirectory(test/consumer)
# javaHeclib only exists when a JDK was found; the load test also needs javac/java.
if(TARGET javaHeclib AND Java_Development_FOUND)
    add_subdirectory(test/jni)
endif()
if(DSS_TEST_DATA_DIR)
    option(RUN_TESTS "Run functional tests, requires dss-test-data git repo." ON)
    if(RUN_TESTS)
        add_subdirectory(test)
    endif()
else()
    message(STATUS "Dss Test Data could not be retrieved -- functional tests skipped.")
endif()

# Package the natives from this build, so the published zips are the exact binaries CI
# built and verified -- not separately produced ones. Two components -> two zips:
#   capi -- the hecdss C API (dll/so/dylib + header + Windows import lib)
#   jni  -- the javaHeclib JNI native a JVM loads (built only when a JDK is present)
# ARCHIVE ships the Windows import library (hecdss.lib): the header exports symbols
# with __declspec(dllimport), so without it a consumer can't link against the DLL.
# COMPONENT must be repeated per artifact kind: a single trailing COMPONENT binds only
# to the last block, silently dropping the .so/.dll from the package.
install(TARGETS hecdss
    RUNTIME DESTINATION . COMPONENT capi   # hecdss.dll (Windows)
    LIBRARY DESTINATION . COMPONENT capi   # libhecdss.so / .dylib (Linux/macOS)
    ARCHIVE DESTINATION . COMPONENT capi)  # hecdss.lib import library (Windows)
install(FILES heclib/hecdss/hecdss.h DESTINATION . COMPONENT capi)
if(TARGET javaHeclib)
    install(TARGETS javaHeclib
        RUNTIME DESTINATION . COMPONENT jni
        LIBRARY DESTINATION . COMPONENT jni
        ARCHIVE DESTINATION . COMPONENT jni)
endif()

# Version the published zip (HECDSS_VERSION parsed above) so an artifact is
# self-identifying once detached from CI.
# Classifier (e.g. darwin-aarch64) is passed by CI so the zip names the platform too;
# falls back to CMake's own arch/system names for a plain local build.
if(NOT HECDSS_PACKAGE_CLASSIFIER)
    set(HECDSS_PACKAGE_CLASSIFIER "${CMAKE_SYSTEM_NAME}-${CMAKE_SYSTEM_PROCESSOR}")
endif()

set(CPACK_GENERATOR "ZIP")
set(CPACK_ARCHIVE_COMPONENT_INSTALL ON)        # one zip per component, not one combined
set(CPACK_INCLUDE_TOPLEVEL_DIRECTORY OFF)      # contents at the zip root
set(CPACK_COMPONENTS_ALL capi)
set(CPACK_ARCHIVE_CAPI_FILE_NAME "hecdss-${HECDSS_VERSION}-${HECDSS_PACKAGE_CLASSIFIER}")
if(TARGET javaHeclib)
    list(APPEND CPACK_COMPONENTS_ALL jni)
    set(CPACK_ARCHIVE_JNI_FILE_NAME "javaHeclib-${HECDSS_VERSION}-${HECDSS_PACKAGE_CLASSIFIER}")
endif()
include(CPack)
