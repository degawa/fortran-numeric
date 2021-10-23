set(LIBNAME ${PROJECT_NAME})

# generating source files using fypp
set(FYPP_SRCS
    integer
    real
    nonNumber
)

set(F90_SRCS)
foreach(SRC IN LISTS FYPP_SRCS)
    add_custom_command(
        OUTPUT ${CMAKE_CURRENT_SOURCE_DIR}/${SRC}/${SRC}.f90
        COMMAND fypp ${CMAKE_CURRENT_SOURCE_DIR}/${SRC}/${SRC}.fy90 ${CMAKE_CURRENT_SOURCE_DIR}/${SRC}/${SRC}.f90
        MAIN_DEPENDENCY ${CMAKE_CURRENT_SOURCE_DIR}/${SRC}/${SRC}.fy90
    )
endforeach()

add_library(${LIBNAME} STATIC)
target_sources(${LIBNAME}
    PUBLIC
    integer/integer.f90
    real/real.f90
    nonNumber/nonNumber.f90
)

set_target_properties(${LIBNAME}
    PROPERTIES
    ARCHIVE_OUTPUT_DIRECTORY ${LIB_OUTPUT_DIR}
    LIBRARY_OUTPUT_DIRECTORY ${LIB_OUTPUT_DIR}
    Fortran_MODULE_DIRECTORY ${MOD_OUTPUT_DIR}
)


# add_subdirectory(tests)