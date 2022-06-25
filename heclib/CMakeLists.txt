
add_subdirectory(heclib_f)
add_subdirectory(heclib_c)

add_library(heclib STATIC $<TARGET_OBJECTS:heclib_c> $<TARGET_OBJECTS:heclib_f>)
target_link_libraries(heclib PRIVATE ZLIB::ZLIB)
target_include_directories(heclib PUBLIC ${CMAKE_CURRENT_LIST_DIR}/heclib_c/src/headers)
target_include_directories(heclib PUBLIC ${CMAKE_CURRENT_LIST_DIR}/heclib_f/src/headers)

add_subdirectory(javaHeclib)