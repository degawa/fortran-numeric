cmake_minimum_required(VERSION 3.15)

enable_language(Fortran)

project(fortran-numeric Fortran)

enable_testing()

if(CMAKE_Fortran_COMPILER_ID MATCHES GNU OR Fortran_COMPILER_NAME MATCHES "gfortran*")
    set(CMAKE_Fortran_FLAGS         "-ffree-line-length-none -fimplicit-none -std=f2018 -cpp")
    set(CMAKE_Fortran_FLAGS_DEBUG   "-Wall -Wno-maybe-uninitialized -fbacktrace -fcheck=all,no-array-temps -O0 -g")
    set(CMAKE_Fortran_FLAGS_RELEASE "-O3 -march=native")
endif()
if(CMAKE_Fortran_COMPILER_ID MATCHES Intel OR Fortran_COMPILER_NAME MATCHES "ifort*")
    if(CMAKE_Fortran_COMPILER_VERSION VERSION_LESS 19.0)
        set(STD_FLAG_KEYWORD "f15")
    else()
        set(STD_FLAG_KEYWORD "f18")
    endif()

    if(CMAKE_SYSTEM_NAME STREQUAL "Windows")
        set(CMAKE_Fortran_FLAGS         "/stand:${STD_FLAG_KEYWORD} /fpp /nologo")
        set(CMAKE_Fortran_FLAGS_DEBUG   "/warn:all /Qdiag-disable:5462 /Qdiag-disable:5194 /traceback /check:all /check:noarg_temp_created /debug:full /Od")
        set(CMAKE_Fortran_FLAGS_RELEASE "/O3 /QxHost")
    else()
        set(CMAKE_Fortran_FLAGS         "-stand ${STD_FLAG_KEYWORD} -fpp -nologo")
        set(CMAKE_Fortran_FLAGS_DEBUG   "-warn all -traceback -check all,noarg_temp_created -debug full -O0")
        set(CMAKE_Fortran_FLAGS_RELEASE "-O3 -xHost")
    endif()
endif()

# ディレクトリ設定
set(MOD_OUTPUT_DIR ${PROJECT_BINARY_DIR}/mod)
set(LIB_OUTPUT_DIR ${PROJECT_BINARY_DIR}/lib)

message("module  file output dir: ${MOD_OUTPUT_DIR}")
message("library file output dir: ${LIB_OUTPUT_DIR}")

add_subdirectory(src)
