set(LIBNAME ${PROJECT_NAME})

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


add_subdirectory(tests)