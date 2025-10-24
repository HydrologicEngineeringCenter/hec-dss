file(GLOB C_SRC LIST_DIRECTORIES true source/*.c)
add_executable(Dss-C TestDssC.c ${C_SRC} test_logging.c TestDssC.h TestDssC.c)
target_link_libraries(Dss-C heclib)
target_include_directories(Dss-C PRIVATE ${CMAKE_CURRENT_SOURCE_DIR})
add_custom_target(
    copy-test-data-dss-c ALL
    COMMAND ${CMAKE_COMMAND} -E copy_directory "${DSS_TEST_DATA_DIR}/" "${CMAKE_CURRENT_BINARY_DIR}/test"
    DEPENDS ${DSS_TEST_DATA_DIR}
)


add_test(NAME Dss-C COMMAND Dss-C test WORKING_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/test")
if (MSVC)
set_property(TEST Dss-C PROPERTY ENVIRONMENT_MODIFICATION "PATH=path_list_append:$<TARGET_RUNTIME_DLL_DIRS:Dss-C>")
endif()
