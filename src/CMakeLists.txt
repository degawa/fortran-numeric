set(LIBNAME ${PROJECT_NAME})

# generating source files using fypp
set(FYPP_SRCS
    integer
    real
    nonNumber
)

foreach(SRC IN LISTS FYPP_SRCS)
    add_custom_command(
        OUTPUT ${CMAKE_CURRENT_SOURCE_DIR}/${SRC}/${SRC}.f90
        COMMAND fypp ${CMAKE_CURRENT_SOURCE_DIR}/${SRC}/${SRC}.fy90 ${CMAKE_CURRENT_SOURCE_DIR}/${SRC}/${SRC}.f90
        MAIN_DEPENDENCY ${CMAKE_CURRENT_SOURCE_DIR}/${SRC}/${SRC}.fy90
    )
endforeach()

add_library(${LIBNAME}
    integer/integer.f90
    real/real.f90
    nonNumber/nonNumber.f90
)

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
