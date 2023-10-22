set(LIBNAME ${PROJECT_NAME})

if(NOT TARGET "test-drive::test-drive")
    find_package("test-drive" REQUIRED)
endif()

function(ADDTEST name)
    add_executable(test_${name} test_${name}.f90)

    target_link_libraries(test_${name}
        ${LIBNAME}
        "test-drive::test-drive"
    )

    add_test(NAME ${name}
        COMMAND $<TARGET_FILE:test_${name}> ${CMAKE_CURRENT_BINARY_DIR}
        WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
    )
endfunction(ADDTEST)

add_subdirectory(integer)
add_subdirectory(real)
add_subdirectory(nonNumber)
