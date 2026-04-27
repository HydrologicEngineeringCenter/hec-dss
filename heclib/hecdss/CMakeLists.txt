

add_library(hecdss SHARED)

heclib_set_compile_options(hecdss)
if(MSVC)
    target_compile_definitions(hecdss PRIVATE HECDSS_EXPORTS)
endif()

target_link_libraries(hecdss heclib)
target_sources(hecdss PRIVATE hecdss.c)
target_sources(hecdss PUBLIC hecdss.h)
