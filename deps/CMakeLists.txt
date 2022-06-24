include(FetchContent)

FetchContent_Declare(
    zlib
    GIT_REPOSITORY https://github.com/madler/zlib.git
    GIT_TAG v1.2.12
    PATCH_COMMAND git apply --reject --whitespace=fix ${CMAKE_CURRENT_SOURCE_DIR}/zlib/build.patch
)

add_subdirectory(zlib)
