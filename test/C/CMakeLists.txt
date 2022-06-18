

add_executable(ts_readv6 ts_readv6.c getopt.c)
configure_file(${PROJECT_SOURCE_DIR}/dss-test-data/v6-pc.dss v6-pc.dss COPYONLY)
#set_property(TARGET ts_readv6 PROPERTY LINKER_LANGUAGE Fortran)
target_link_libraries(ts_readv6 heclib)
add_test(NAME ts_readv6 COMMAND ts_readv6)