add_library(check STATIC)
target_sources(check
    PRIVATE
    check.f90
)

set_target_properties(check
    PROPERTIES
    Fortran_MODULE_DIRECTORY ${MOD_OUTPUT_DIR}
)

function(ADDTEST name)
    add_executable(test_${name} test_${name}.f90)

    target_include_directories(test_${name}
        PRIVATE
        ${MOD_OUTPUT_DIR}
    )
    target_link_libraries(test_${name}
        PRIVATE
        check
        ${LIBNAME}
    )

    add_test(NAME ${name}
             COMMAND $<TARGET_FILE:test_${name}> ${CMAKE_CURRENT_BINARY_DIR}
             WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
    )
endfunction(ADDTEST)

add_subdirectory(integer)
