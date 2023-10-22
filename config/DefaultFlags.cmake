if(CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")
    set(
        CMAKE_Fortran_FLAGS_INIT
        "-fimplicit-none"
        "-ffree-line-length-none"
        "-std=f2018"
        "-cpp"
    )
    set(
        CMAKE_Fortran_FLAGS_RELEASE_INIT
        "-O3"
        "-march=native"
    )
    set(
        CMAKE_Fortran_FLAGS_DEBUG_INIT
        "-Wall"
        "-Wextra"
        "-Wimplicit-procedure"
        "-O0"
        "-g"
        "-fbacktrace"
        "-fcheck=all,no-array-temps"
    )
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "^Intel")
    if(WIN32)
        set(
            CMAKE_Fortran_FLAGS_INIT
            "/stand:f18"
            "/warn:declarations,general,usage,interfaces,unused"
        )
    else()
        set(
            CMAKE_Fortran_FLAGS_INIT
            "-stand f18"
            "-warn declarations,general,usage,interfaces,unused"
        )
    endif()

    if(WIN32)
        set(
            CMAKE_Fortran_FLAGS_RELEASE_INIT
            "/O3"
            "/QxHost"
        )
        set(
            CMAKE_Fortran_FLAGS_DEBUG_INIT
            "/warn:all"
            "/Qdiag-disable:5462"
            "/Qdiag-disable:5194"
            "/traceback"
            "/check:all"
            "/check:noarg_temp_created"
            "/debug:full"
            "/Od"
        )
    else()
        set(
            CMAKE_Fortran_FLAGS_RELEASE_INIT
            "-O3"
            "-xHost"
        )
        set(
            CMAKE_Fortran_FLAGS_DEBUG_INIT
            "-warn all"
            "-traceback"
            "-check all,noarg_temp_created"
            "-debug full"
            "-O0"
        )
    endif()
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "NAG")
    set(
        CMAKE_Fortran_FLAGS_INIT
        "-fpp"
        "-f2018"
        "-ieee=full"
    )
    set(
        CMAKE_Fortran_FLAGS_RELEASE_INIT
        "-O3"
    )
    set(
        CMAKE_Fortran_FLAGS_DEBUG_INIT
        "-g"
        "-nan"
        "-C=all"
        "-mtrace=on"
        "-O0"
        "-pg"
    )
else()
    set(
        CMAKE_Fortran_FLAGS_INIT
    )
    set(
        CMAKE_Fortran_FLAGS_RELEASE_INIT
    )
    set(
        CMAKE_Fortran_FLAGS_DEBUG_INIT
    )
endif()

string(REPLACE ";" " " CMAKE_Fortran_FLAGS_INIT "${CMAKE_Fortran_FLAGS_INIT}")
string(REPLACE ";" " " CMAKE_Fortran_FLAGS_RELEASE_INIT "${CMAKE_Fortran_FLAGS_RELEASE_INIT}")
string(REPLACE ";" " " CMAKE_Fortran_FLAGS_DEBUG_INIT "${CMAKE_Fortran_FLAGS_DEBUG_INIT}")