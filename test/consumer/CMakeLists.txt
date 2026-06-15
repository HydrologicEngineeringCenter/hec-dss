# Smoke-test libhecdss through its public boundary: link the SHARED hecdss target (so
# the Windows import library, symbol visibility, and runtime load are all exercised)
# and call the exported API. Unlike the heclib tests, this consumes what we ship.
# Needs only the built lib, so it runs on every build alongside the self_contained check.
add_executable(consume_hecdss consume_hecdss.c)
target_link_libraries(consume_hecdss hecdss)
target_include_directories(consume_hecdss PRIVATE ${CMAKE_SOURCE_DIR}/heclib/hecdss)
target_compile_definitions(consume_hecdss PRIVATE EXPECTED_VERSION="${HECDSS_VERSION}")
add_test(NAME consume_hecdss COMMAND consume_hecdss)

# Windows: the test exe needs hecdss.dll discoverable at run time (mirrors the other tests).
if(MSVC)
    set_property(TEST consume_hecdss PROPERTY ENVIRONMENT_MODIFICATION
        "PATH=path_list_append:$<TARGET_RUNTIME_DLL_DIRS:consume_hecdss>")
endif()
