# Fail if the native at -DLIB links anything outside the OS baseline.
# Run as: cmake -DLIB=<path-to-native> -P verify_self_contained.cmake
#
# file(GET_RUNTIME_DEPENDENCIES) resolves the runtime deps; we exclude the OS system
# directories and re-flag the libraries we static-link, so a shared zlib / VC++ runtime
# regression or the macOS @rpath load failure still fails the build.
cmake_minimum_required(VERSION 3.16)

if(NOT DEFINED LIB)
    message(FATAL_ERROR "usage: cmake -DLIB=<path-to-native> -P verify_self_contained.cmake")
endif()

# Windows API-set DLLs are virtual forwarders with no file on disk, so the
# directory-based POST_EXCLUDE can't reach them; drop them before resolution.
if(WIN32)
    set(_win_api_set_excludes PRE_EXCLUDE_REGEXES "^api-ms-" "^ext-ms-")
endif()

file(GET_RUNTIME_DEPENDENCIES
    LIBRARIES "${LIB}"
    RESOLVED_DEPENDENCIES_VAR resolved
    UNRESOLVED_DEPENDENCIES_VAR unresolved
    ${_win_api_set_excludes}
    # Re-flag libraries we static-link, so a regression to a shared one is still caught.
    POST_INCLUDE_REGEXES ".*/libz\\." ".*[Vv][Cc][Rr][Uu][Nn][Tt][Ii][Mm][Ee].*" ".*[Mm][Ss][Vv][Cc][Pp].*"
    # Everything else the OS provides lives in these directories.
    POST_EXCLUDE_REGEXES "^/lib" "^/usr/lib" "^/System/Library" "[Ss]ystem32"
)

message(STATUS "resolved (non-OS, would need bundling): ${resolved}")
message(STATUS "unresolved (cannot load off build box): ${unresolved}")
if(resolved OR unresolved)
    message(FATAL_ERROR "Native is not self-contained -- links non-OS libraries.")
endif()
message(STATUS "PASS: native links only OS-baseline libraries")
