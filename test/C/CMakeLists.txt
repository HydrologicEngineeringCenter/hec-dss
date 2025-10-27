
add_library(getopt OBJECT getopt.c)


add_executable(ts_write ts_write.c $<TARGET_OBJECTS:getopt>)
target_link_libraries(ts_write heclib)
add_test(NAME ts_write COMMAND ts_write)


add_executable(ts_write_irregular ts_write_irregular.c $<TARGET_OBJECTS:getopt>)
target_link_libraries(ts_write_irregular heclib)
add_test(NAME ts_write_irregular COMMAND ts_write_irregular)


add_executable(ExampleSecondGranularity ExampleSecondGranularity.c $<TARGET_OBJECTS:getopt>)
target_link_libraries(ExampleSecondGranularity heclib)
add_test(NAME ExampleSecondGranularity COMMAND ExampleSecondGranularity)


add_executable(SampleText1 SampleText1.c $<TARGET_OBJECTS:getopt>)
target_link_libraries(SampleText1 heclib)
add_test(NAME SampleText1 COMMAND SampleText1)


add_executable(SamplePairedData SamplePairedData.c $<TARGET_OBJECTS:getopt>)
target_link_libraries(SamplePairedData heclib)
add_test(NAME SamplePairedData COMMAND SamplePairedData)


add_executable(ExampleMinuteGranularity ExampleMinuteGranularity.c $<TARGET_OBJECTS:getopt>)
target_link_libraries(ExampleMinuteGranularity heclib)
add_test(NAME ExampleMinuteGranularity COMMAND ExampleMinuteGranularity)


add_executable(ExampleTimeSeries1 ExampleTimeSeries1.c $<TARGET_OBJECTS:getopt>)
target_link_libraries(ExampleTimeSeries1 heclib)
add_test(NAME ExampleTimeSeries1 COMMAND ExampleTimeSeries1)


add_executable(ExampleTimeSeries2 ExampleTimeSeries2.c $<TARGET_OBJECTS:getopt>)
target_link_libraries(ExampleTimeSeries2 heclib)
add_test(NAME ExampleTimeSeries2 COMMAND ExampleTimeSeries2)

add_executable(GridTest GridTest.c $<TARGET_OBJECTS:getopt>)
target_link_libraries(GridTest heclib)
add_test(NAME GridTest COMMAND GridTest -d GridTest.dss -h 100 -w 100 -c 26 -r 12)
add_test(NAME GridTestVerify COMMAND GridTest -d GridTest.dss -h 100 -w 100 -c 26 -r 12 -v)

add_executable(zopenExample zopenExample.c $<TARGET_OBJECTS:getopt>)
target_link_libraries(zopenExample heclib)
add_test(NAME zopenExample COMMAND zopenExample)


if(MSVC)
set_property(TEST ts_write_irregular PROPERTY ENVIRONMENT_MODIFICATION "PATH=path_list_append:$<TARGET_RUNTIME_DLL_DIRS:ts_write_irregular>")
set_property(TEST ts_write PROPERTY ENVIRONMENT_MODIFICATION "PATH=path_list_append:$<TARGET_RUNTIME_DLL_DIRS:ts_write>")
set_property(TEST ExampleSecondGranularity PROPERTY ENVIRONMENT_MODIFICATION "PATH=path_list_append:$<TARGET_RUNTIME_DLL_DIRS:ExampleSecondGranularity>")
set_property(TEST SampleText1 PROPERTY ENVIRONMENT_MODIFICATION "PATH=path_list_append:$<TARGET_RUNTIME_DLL_DIRS:SampleText1>")
set_property(TEST SamplePairedData PROPERTY ENVIRONMENT_MODIFICATION "PATH=path_list_append:$<TARGET_RUNTIME_DLL_DIRS:SamplePairedData>")
set_property(TEST ExampleMinuteGranularity PROPERTY ENVIRONMENT_MODIFICATION "PATH=path_list_append:$<TARGET_RUNTIME_DLL_DIRS:ExampleMinuteGranularity>")
set_property(TEST ExampleTimeSeries1 PROPERTY ENVIRONMENT_MODIFICATION "PATH=path_list_append:$<TARGET_RUNTIME_DLL_DIRS:ExampleTimeSeries1>")
set_property(TEST ExampleTimeSeries2 PROPERTY ENVIRONMENT_MODIFICATION "PATH=path_list_append:$<TARGET_RUNTIME_DLL_DIRS:ExampleTimeSeries2>")
set_property(TEST GridTest PROPERTY ENVIRONMENT_MODIFICATION "PATH=path_list_append:$<TARGET_RUNTIME_DLL_DIRS:GridTest>")
set_property(TEST GridTestVerify PROPERTY ENVIRONMENT_MODIFICATION "PATH=path_list_append:$<TARGET_RUNTIME_DLL_DIRS:GridTest>")

set_property(TEST zopenExample PROPERTY ENVIRONMENT_MODIFICATION "PATH=path_list_append:$<TARGET_RUNTIME_DLL_DIRS:zopenExample>")
endif()