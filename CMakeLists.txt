cmake_minimum_required(VERSION 3.15)

enable_language(Fortran)

project(fortran-numeric LANGUAGES Fortran)

set(CMAKE_USER_MAKE_RULES_OVERRIDE ${CMAKE_CURRENT_SOURCE_DIR}/config/DefaultFlags.cmake)

enable_testing()

# ディレクトリ設定
set(MOD_OUTPUT_DIR ${PROJECT_BINARY_DIR}/mod)
set(LIB_OUTPUT_DIR ${PROJECT_BINARY_DIR}/lib)

message("module  file output dir: ${MOD_OUTPUT_DIR}")
message("library file output dir: ${LIB_OUTPUT_DIR}")

add_subdirectory(src)
