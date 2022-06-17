FetchContent_MakeAvailable(zlib)
target_include_directories(zlibstatic PUBLIC ${zlib_SOURCE_DIR} ${zlib_BINARY_DIR})
add_library(ZLIB::ZLIB ALIAS zlibstatic)