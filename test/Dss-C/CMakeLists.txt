file(GLOB C_SRC LIST_DIRECTORIES true source/*.c)
add_executable(Dss-C TestDssC.c ${C_SRC})
target_link_libraries(Dss-C heclib)
target_include_directories(Dss-C PRIVATE ${CMAKE_CURRENT_SOURCE_DIR})
add_custom_target(
    copy-test-data-dss-c ALL
    COMMAND ${CMAKE_COMMAND} -E copy_directory "${CMAKE_SOURCE_DIR}/dss-test-data" "${CMAKE_CURRENT_BINARY_DIR}/test"
    DEPENDS ${PROJECT_SOURCE_DIR}/dss-test-data
)


add_test(NAME Dss-C COMMAND Dss-C test)