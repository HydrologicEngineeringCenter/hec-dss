
add_executable(Dss-C TestDssC.c)
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

target_sources(Dss-C PRIVATE
test_logging.c
TestDssC.h
TestDssC.c
source/testTimeSeries5.c
source/testReclaim.c
source/testProfileIrreg.c
source/Bulletin_17C_SSP_Issue.c
source/testPairedData4.c
source/testDateTime.c
source/testArrayWriteRead.c
source/testInternalIO.c
source/testTextTable.c
source/testAlias.c
source/runTests.c
source/TestUtility.c
source/testMisc.c
source/findInFile.c
source/compareProfiles.c
source/testZtsProcessTimes.c
source/testSaveTimeSeriesEmptyBlocks.c
source/testTimeSeriesStruct11.c
source/compareTss.c
source/testTimeSeriesStruct14.c
source/profileExcel.c
source/preCompressionGridTest.c
source/testVerticalDatum_c.c
source/testText.c
source/testTimeSeriesStruct1.c
source/testTimeSeriesStruct2.c
source/testTimeSeriesStruct15.c
source/testDelete.c
source/addLocation.c
source/testDateFunctions.c
source/testMultiUser.c
source/testLargeCopy.c
source/testBufferedIO.c
source/TestPairedData3.c
source/testLocation.c
source/testCatalog.c
source/testTimeSeriesStruct3.c
source/testNormalizeFPart.c
source/comparePDs.c
source/current_time_testing.c
source/testTimeSeriesStruct4.c
source/SampleTimeSeries3.c
source/testPairedData.c
source/testTin.c
source/testExpandedTimesIrreg2.c
source/testztsStructBill.c
source/testTimeSeriesPattern.c
source/testExpandedTimes.c
source/TestPairedData2.c
source/testTimeSeriesStruct13.c
source/testTimeSeriesStruct5.c
source/PathnameTesting.c
source/testExpandedTimesIrreg.c
source/mixed_record_types.c
source/testAdHoc.c
source/testTimeSeriesStruct12.c
source/test_stringCat.c
source/miscTests.c
source/testSqueeze.c
source/testWriteRead.c
source/testNoDates.c
source/testProfileReg.c
)
