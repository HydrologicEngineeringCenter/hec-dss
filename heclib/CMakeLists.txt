
add_subdirectory(heclib_c)

add_library(heclib STATIC $<TARGET_OBJECTS:heclib_c>)


if(MSVC)
    target_link_libraries(heclib PRIVATE zlibstatic)
else()
    target_link_libraries(heclib PRIVATE ZLIB::ZLIBSTATIC)
endif()

target_include_directories(heclib PUBLIC ${CMAKE_CURRENT_LIST_DIR}/heclib_c/src/headers)

add_subdirectory(hecdss)
add_subdirectory(javaHeclib)