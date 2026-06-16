FetchContent_MakeAvailable(zlib)
# Bundle zlib statically (madler/zlib: `zlib` is shared, `zlibstatic` static) so the
# shipped libhecdss doesn't depend on @rpath/libz.1.dylib, which fails to resolve off
# the build machine on macOS. PIC lets the archive link into the shared lib on Linux.
set_target_properties(zlibstatic PROPERTIES POSITION_INDEPENDENT_CODE ON)
target_include_directories(zlibstatic PUBLIC ${zlib_SOURCE_DIR} ${zlib_BINARY_DIR})
add_library(ZLIB::ZLIBSTATIC ALIAS zlibstatic)
