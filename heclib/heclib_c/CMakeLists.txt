file(GLOB_RECURSE C_SRC LIST_DIRECTORIES true src/*.c)

add_library(heclib_c OBJECT ${C_SRC})
target_compile_options(heclib_c PRIVATE -fPIC -std=gnu99)
target_include_directories(heclib_c PUBLIC src/headers)
target_link_libraries(heclib_c PRIVATE ZLIB::ZLIB)
