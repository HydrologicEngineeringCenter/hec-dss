

add_executable(ts_readv6 ts_readv6.c getopt.c)
set_property(TARGET ts_readv6 PROPERTY LINKER_LANGUAGE Fortran)
target_link_libraries(ts_readv6 heclib)
add_test(NAME ts_readv6 COMMAND ${CMAKE_COMMAND} -E ts_readv6)