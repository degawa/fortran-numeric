cmake_minimum_required(VERSION 3.15)

enable_language(Fortran)

project(fortran-numeric LANGUAGES Fortran)

# Read version from file
file(STRINGS "${PROJECT_SOURCE_DIR}/VERSION" PROJECT_VERSION)
string(REPLACE "." ";" VERSION_LIST ${PROJECT_VERSION})
list(GET VERSION_LIST 0 PROJECT_VERSION_MAJOR)
list(GET VERSION_LIST 1 PROJECT_VERSION_MINOR)
list(GET VERSION_LIST 2 PROJECT_VERSION_PATCH)
unset(VERSION_LIST)

set(CMAKE_USER_MAKE_RULES_OVERRIDE ${CMAKE_CURRENT_SOURCE_DIR}/config/DefaultFlags.cmake)

# Follow GNU conventions for installation directories
include(GNUInstallDirs)

# --- CMake specific configuration and package data export
add_subdirectory(config)

find_program(FYPP fypp)

if(NOT FYPP)
  message(FATAL_ERROR "fypp not found")
endif()

add_subdirectory(src)

# enable build testing
include(CTest)

if(BUILD_TESTING)
  enable_testing()
  add_subdirectory(test)
endif()

install(EXPORT ${PROJECT_NAME}-targets
  NAMESPACE ${PROJECT_NAME}::
  DESTINATION "${CMAKE_INSTALL_LIBDIR}/cmake/${PROJECT_NAME}"
)
