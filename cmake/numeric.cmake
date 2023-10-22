# Preprocesses a list of files with given preprocessor and preprocessor options
#
# Args:
# preproc [in]: Preprocessor program
# preprocopts [in]: Preprocessor options
# srcext [in]: File extension of the source files
# trgext [in]: File extension of the target files
# srcfiles [in]: List of the source files
# trgfiles [out]: Contains the list of the preprocessed files on exit
#
function(preprocess preproc preprocopts srcext trgext srcfiles trgfiles)
    set(_trgfiles)

    foreach(srcfile IN LISTS srcfiles)
        string(REGEX REPLACE "\\.${srcext}$" ".${trgext}" trgfile ${srcfile})

        # extract directory name from trgfile like "dir/src.f90"
        string(REGEX MATCH "^(.+/).+$" extracted_dir "${trgfile}")

        if(extracted_dir)
            set(DIR_NAME ${CMAKE_CURRENT_BINARY_DIR}/${CMAKE_MATCH_1})

            if(NOT EXISTS "${DIR_NAME}")
                message("making directory: ${DIR_NAME}")
                make_directory("${DIR_NAME}")
            endif()
        endif()

        add_custom_command(
            OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/${trgfile}
            COMMAND ${preproc} ${preprocopts} ${CMAKE_CURRENT_SOURCE_DIR}/${srcfile} ${CMAKE_CURRENT_BINARY_DIR}/${trgfile}
            MAIN_DEPENDENCY ${CMAKE_CURRENT_SOURCE_DIR}/${srcfile})
        list(APPEND _trgfiles ${CMAKE_CURRENT_BINARY_DIR}/${trgfile})
    endforeach()

    set(${trgfiles} ${_trgfiles} PARENT_SCOPE)
endfunction()

# Preprocesses fortran files with fypp.
#
# It assumes that source files have the ".fy90" extension. Target files will be
# created with the extension ".f90". The FYPP variable must contain the path to
# the fypp-preprocessor.
#
# Args:
# fyppopts [in]: Options to pass to fypp.
# fyppfiles [in]: Files to be processed by fypp
# f90files [out]: List of created f90 files on exit
#
function(fypp_f90 fyppopts fyppfiles f90files)
    preprocess("${FYPP}" "${fyppopts}" "fy90" "f90" "${fyppfiles}" _f90files)
    set(${f90files} ${_f90files} PARENT_SCOPE)
endfunction()
