
add_subdirectory(heclib_f)
add_subdirectory(heclib_c)

add_library(heclib $<TARGET_OBJECTS:heclib_c> $<TARGET_OBJECTS:heclib_f>)
target_link_libraries(heclib PRIVATE ZLIB::ZLIB)
target_include_directories(heclib PUBLIC ${CMAKE_CURRENT_LIST_DIR}/heclib_c/src/headers)
if(MSVC)
    set(CMAKE_LINKER_FLAGS /NODEFAULTLIB:MSVCRT /NODEFAULTLIB:MSVCRTD /NODEFAULTLIB:LIBCMTD )
endif()