project(heclib LANGUAGES C Fortran)
cmake_minimum_required(VERSION 3.16)



if(MSVC)
    set(CMAKE_EXE_LINKER_FLAGS /NODEFAULTLIB:MSVCRTD,LIBCMT)
endif()

add_subdirectory(deps)
add_subdirectory(heclib)

find_package(Git QUIET)
if(GIT_FOUND AND EXISTS "${PROJECT_SOURCE_DIR}/.git")
    option(RUN_TESTS "Run Tests, requires initializing a git submodule." ON)
    if(RUN_TESTS)

        execute_process(COMMAND ${GIT_EXECUTABLE} submodule update --init --recursive
                        WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
                        RESULT_VARIABLE GIT_SUBMOD_RESULT
        )
        if( NOT GIT_SUBMOD_RESULT EQUAL "0")
            message(FATAL_ERROR "could not retreive test data. Error ${GIT_SUBMOD_RESULT}")
        endif()
        enable_testing()
        add_subdirectory(test)
    endif()
else()
    message(STATUS "git not found. Tests require git submodule initialization to build. Please install git if you wish to run the tests.")
endif()

