set(LIBNAME ${PROJECT_NAME})

include(${PROJECT_SOURCE_DIR}/cmake/numeric.cmake)

# generating source files using fypp
set(FYPP_SRCS
    integer/integer.fy90
    real/real.fy90
    nonNumber/nonNumber.fy90
)

fypp_f90("${FYPP_FLAGS}" "${FYPP_SRCS}" F90_SCRS)

add_library(${LIBNAME} ${F90_SCRS})

set(MOD_OUTPUT_DIR ${CMAKE_CURRENT_BINARY_DIR}/mod)

set_target_properties(${LIBNAME}
    PROPERTIES
    POSITION_INDEPENDENT_CODE ON
    WINDOWS_EXPORT_ALL_SYMBOLS ON
    Fortran_MODULE_DIRECTORY ${MOD_OUTPUT_DIR}
)

target_include_directories(${LIBNAME} PUBLIC
    $<BUILD_INTERFACE:${MOD_OUTPUT_DIR}>
    $<INSTALL_INTERFACE:${CMAKE_INSTALL_MODULEDIR}>
)

install(TARGETS ${LIBNAME}
    EXPORT ${LIBNAME}-targets
    ARCHIVE DESTINATION "${CMAKE_INSTALL_LIBDIR}"
    LIBRARY DESTINATION "${CMAKE_INSTALL_LIBDIR}"
)
install(DIRECTORY ${MOD_OUTPUT_DIR} DESTINATION "${CMAKE_INSTALL_MODULEDIR}")
