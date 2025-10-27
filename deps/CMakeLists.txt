include(FetchContent)

FetchContent_Declare(
    zlib
    GIT_REPOSITORY https://github.com/madler/zlib.git
    GIT_TAG v1.3.1
)
set(ZLIB_BUILD_STATIC ON)
add_subdirectory(zlib)

FetchContent_Declare(
    dss_test_data
    GIT_REPOSITORY https://github.com/HydrologicEngineeringCenter/dss-test-data.git
    GIT_TAG master
    CONFIGURE_COMMAND ""
    BUILD_COMMAND ""
)

FetchContent_MakeAvailable(dss_test_data)
add_custom_target(DSS_TEST_DATA SOURCES ${dss_test_data_SOURCE_DIR})
set(DSS_TEST_DATA_DIR "${dss_test_data_SOURCE_DIR}" CACHE INTERNAL "Location of test data")