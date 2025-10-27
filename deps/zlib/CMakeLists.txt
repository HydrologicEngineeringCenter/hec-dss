FetchContent_MakeAvailable(zlib)
target_include_directories(zlib PUBLIC ${zlib_SOURCE_DIR} ${zlib_BINARY_DIR})
add_library(ZLIB::ZLIBSTATIC ALIAS zlib)