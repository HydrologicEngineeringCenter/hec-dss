#  Self-contained hardening regression tests: link static heclib, no test-data repo needed.
#  Definitive run with ASAN:
#    cmake -B build-asan -DHECDSS_SANITIZE=ON && cmake --build build-asan
#    ctest --test-dir build-asan -L hardening --output-on-failure

add_executable(ts_internal_header_unpack tsInternalHeaderUnpackTest.c)
target_link_libraries(ts_internal_header_unpack heclib)
add_test(NAME ts_internal_header_unpack COMMAND ts_internal_header_unpack)

if(NOT MSVC)
    set_tests_properties(ts_internal_header_unpack PROPERTIES
        LABELS "hardening"
        ENVIRONMENT "ASAN_OPTIONS=detect_leaks=0")
else()
    set_tests_properties(ts_internal_header_unpack PROPERTIES LABELS "hardening")
endif()
