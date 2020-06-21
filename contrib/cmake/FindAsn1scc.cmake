# ----------------------------------------------
# CMake module for the ASN1 Semantix Compiler (asn1scc)
#
#
# This module defines:
# Asn1scc_EXECUTABLE - path to asn1scc executable
# Asn1scc_COMMAND - command needed to run asn1scc (can be a list)
# Asn1scc_WORKS - whether asn1scc works
# Asn1scc_VERSION - Numeric version of asn1scc
# Asn1scc_FOUND - whether or not asn1scc was found
#
# It also defines the following functions (see below for full signatures):
# add_asn1_library() - creates a new library target from an ASN.1 protocol
# wrap_asn1_sources() - creates rules to build ASN.1 source files.

set(Asn1scc_PATHS "")
set(Asn1scc_REQUIRED_VARS )

# find asn1scc
# ----------------------------------------------------------

# try to figure out where Asn1scc may be installed.
if("${CMAKE_HOST_SYSTEM_NAME}" STREQUAL "Windows")

    # on Windows, assume that the user extracted the binaries to Program Files
    file(GLOB Asn1scc_PATHS "C:/Program Files/asn1scc*/")

    # if we found multiple paths, check the one with the highest version number first
    list(SORT Asn1scc_PATHS)
    list(REVERSE Asn1scc_PATHS)
elseif("${CMAKE_HOST_SYSTEM_NAME}" STREQUAL "Linux")

    # our install instructions say to install it here
    set(Asn1scc_PATHS /opt/asn1scc)

endif()

find_program(Asn1scc_EXECUTABLE
    NAMES Asn1f4 Asn1f4.exe asn1 asn1.exe # on Linux it has an exe suffix since it's a Mono program
    PATHS ${Asn1scc_PATHS}
    DOC "Path to the asn1scc executable (named asn1f4 or asn1)")
list(APPEND Asn1scc_REQUIRED_VARS Asn1scc_EXECUTABLE)

# find mono if needed
# ----------------------------------------------------------

set(Asn1scc_DEPS_FOUND TRUE)

if("${CMAKE_HOST_SYSTEM_NAME}" STREQUAL "Windows")
    # asn1scc can be run directly
    set(Asn1scc_COMMAND ${Asn1scc_EXECUTABLE})
else()
    find_program(MONO_EXECUTABLE
        NAMES mono
        DOC "Path to the mono executable (required to run asn1scc)")

    list(APPEND Asn1scc_REQUIRED_VARS MONO_EXECUTABLE)

    set(Asn1scc_COMMAND ${MONO_EXECUTABLE} ${Asn1scc_EXECUTABLE})

    if(NOT MONO_EXECUTABLE)
        set(Asn1scc_DEPS_FOUND FALSE)
    endif()
endif()

# check that asn1scc works
# ----------------------------------------------------------

set(Asn1scc_NEED_TO_CHECK_VERSION TRUE)

if(DEFINED Asn1scc_WORKS AND DEFINED Asn1scc_VERSION)
    if(Asn1scc_WORKS)
        set(Asn1scc_NEED_TO_CHECK_VERSION FALSE)
    endif()
endif()

if((NOT Asn1scc_EXECUTABLE) OR (NOT ${Asn1scc_DEPS_FOUND}))
    set(Asn1scc_NEED_TO_CHECK_VERSION FALSE)
endif()

if(Asn1scc_NEED_TO_CHECK_VERSION)
    execute_process(COMMAND ${Asn1scc_COMMAND} --version
        RESULT_VARIABLE Asn1scc_EXIT_CODE
        OUTPUT_VARIABLE Asn1scc_VERSION_OUTPUT)

    if(Asn1scc_EXIT_CODE EQUAL 0)
        set(Asn1scc_WORKS TRUE CACHE BOOL "Whether asn1scc can be executed" FORCE)
        mark_as_advanced(Asn1scc_WORKS)

        # pare the version output down to just a number
        string(REGEX MATCH "[0-9]+\\.([0-9]+\\.?)*" Asn1scc_VERSION_REGEX_OUT "${Asn1scc_VERSION_OUTPUT}")

        # store version for subsequent runs
        set(Asn1scc_VERSION ${Asn1scc_VERSION_REGEX_OUT} CACHE STRING "asn1scc version detected on the system" FORCE)
        mark_as_advanced(Asn1scc_VERSION)
    endif()

endif()

list(APPEND Asn1scc_REQUIRED_VARS Asn1scc_WORKS)

find_package_handle_standard_args(Asn1scc
    REQUIRED_VARS ${Asn1scc_REQUIRED_VARS}
    VERSION_VAR Asn1scc_VERSION)

# Create a library from an asn.1 protocol.
# This function is safe to call even if asn1scc was not found: If asn1scc is not found 
# and GEN_DIR contains up-to-date C files, then those files will be used as-is.  If 
# you want to disable this behavior, simply set REQUIRED in your find_package(Asn1scc) 
# call so that an error is produced if asn1scc is not found.
# The WORD_SIZE and FP_WORD_SIZE options can be used to add the --word-size and --fp-word-size options
# to asn1scc.  They will take care of adding the needed C compile options for these arguments.
# NOTE: all contents of GEN_DIR will be deleted when ASN.1 generation is performed.
# usage: add_asn1_library(<target name>
#   PROTOCOL <.asn and .acn files...>
#   RULES <one or more of: uPER, XER, ACN>
#   ASN1_OPTIONS <options for asn1scc>
#   GEN_DIR <directory in source or binary dir where C files will be generated>
#   [WORD_SIZE <bytes>]
#   [FP_WORD_SIZE <bytes>]
#   [LIB_TYPE <STATIC, OBJECT or SHARED>])

function(add_asn1_library NAME)

    # parse the arguments needed by add_asn1_library, and leave the rest to be passed down
    cmake_parse_arguments(AAL "" "WORD_SIZE;FP_WORD_SIZE;LIB_TYPE;GEN_DIR" "ASN1_OPTIONS" ${ARGN})

    if("${AAL_GEN_DIR}" STREQUAL "")
        message(FATAL_ERROR "GEN_DIR must be provided!")
    endif()

    # generate compile options
    set(C_COMPILE_DEFINITIONS "")
    set(ASN1_COMPILE_OPTIONS ${AAL_ASN1_OPTIONS})

    if(NOT "${AAL_WORD_SIZE}" STREQUAL "")
        list(APPEND C_COMPILE_DEFINITIONS WORD_SIZE=${AAL_WORD_SIZE})
        list(APPEND ASN1_COMPILE_OPTIONS --word-size ${AAL_WORD_SIZE})
    endif()

    if(NOT "${AAL_FP_WORD_SIZE}" STREQUAL "")
        list(APPEND C_COMPILE_DEFINITIONS FP_WORD_SIZE=${AAL_FP_WORD_SIZE})
        list(APPEND ASN1_COMPILE_OPTIONS --fp-word-size ${AAL_FP_WORD_SIZE})
    endif()

    # generate rules for source files
    wrap_asn1_sources(GENERATED_C_FILES
        ASN1_OPTIONS ${ASN1_COMPILE_OPTIONS}
        GEN_DIR ${AAL_GEN_DIR}
        ${AAL_UNPARSED_ARGUMENTS})

    # build library
    add_library(${NAME} ${AAL_LIB_TYPE} ${GENERATED_C_FILES})

    # set compile options
    target_compile_definitions(${NAME} PUBLIC ${C_COMPILE_DEFINITIONS})
    target_include_directories(${NAME} PUBLIC ${AAL_GEN_DIR})

endfunction(add_asn1_library)

# Lower-level function: create rules to build asn.1 source files.
# OUT_VAR will be populated with the names of .c files to attach to a target.
# This function can be used even if asn1scc was not found:
# If asn1scc is not found and GEN_DIR is in the source directory and contains
# up-to-date C files, then those files will be used as-is. If you want to disable this behavior,
# simply set REQUIRED in your find_package(Asn1scc) call so that an error is produced if asn1scc is not found.
# NOTE: all contents of GEN_DIR will be deleted when ASN.1 generation is performed.
# usage: wrap_asn1_sources(<out var>
#   PROTOCOL <.asn and .acn files...>
#   RULES <one or more of: uPER, XER, ACN>
#   ASN1_OPTIONS <options for asn1scc>
#   GEN_DIR <directory in source or binary dir where C files will be generated>)
function(wrap_asn1_sources OUT_VAR)

    # parse the arguments needed by add_asn1_library, and leave the rest to be passed down
    cmake_parse_arguments(WAS "" "GEN_DIR" "PROTOCOL;RULES;ASN1_OPTIONS" ${ARGN})

    if("${WAS_GEN_DIR}" STREQUAL "")
        message(FATAL_ERROR "GEN_DIR must be provided!")
    endif()

    if("${WAS_GEN_DIR}" STREQUAL "${CMAKE_CURRENT_SOURCE_DIR}")
        # prevent people from deleting their own cmake scripts
        message(FATAL_ERROR "GEN_DIR cannot be CMAKE_CURRENT_SOURCE_DIR!")
    endif()

    if("${WAS_PROTOCOL}" STREQUAL "")
        message(FATAL_ERROR "PROTOCOL must be provided!")
    endif()

    if("${WAS_RULES}" STREQUAL "")
        message(FATAL_ERROR "RULES must be provided!")
    endif()

    # source that is always generated
    set(GENERATED_C_SOURCE_PATHS
        ${WAS_GEN_DIR}/asn1crt.c
        ${WAS_GEN_DIR}/asn1crt.h
        ${WAS_GEN_DIR}/asn1crt_encoding.c
        ${WAS_GEN_DIR}/asn1crt_encoding.h)

    # process ASN file names
    set(ASN1_DISPLAY_MODULES_LIST )
    foreach(ASN1_MODULE ${WAS_PROTOCOL})

        # if(EXISTS) can only check absolute paths
        get_filename_component(ASN1_MODULE_FULLPATH "${ASN1_MODULE}" ABSOLUTE)
        if(NOT EXISTS "${ASN1_MODULE_FULLPATH}")
            message(FATAL_ERROR "Cannot find ASN.1 module definition file ${ASN1_MODULE}")
        endif()

        get_filename_component(ASN1_MODULE_FILENAME "${ASN1_MODULE}" NAME)
        string(APPEND ASN1_DISPLAY_MODULES_LIST " ${ASN1_MODULE_FILENAME}")

        get_filename_component(ASN1_MODULE_BASENAME "${ASN1_MODULE}" NAME_WLE)
        list(APPEND GENERATED_C_SOURCE_PATHS
            ${WAS_GEN_DIR}/${ASN1_MODULE_BASENAME}.c
            ${WAS_GEN_DIR}/${ASN1_MODULE_BASENAME}.h)
    endforeach()


    # parse protocols
    set(RULES_OPTIONS)
    if("${WAS_RULES}" MATCHES "uPER")
        list(APPEND RULES_OPTIONS -uPER)
        list(APPEND GENERATED_C_SOURCE_PATHS
            ${WAS_GEN_DIR}/asn1crt_encoding_uper.c
            ${WAS_GEN_DIR}/asn1crt_encoding_uper.h)
    endif()
    if("${WAS_RULES}" MATCHES "XER")
        list(APPEND RULES_OPTIONS -XER)
        list(APPEND GENERATED_C_SOURCE_PATHS
            ${WAS_GEN_DIR}/asn1crt_encoding_xer.c
            ${WAS_GEN_DIR}/asn1crt_encoding_xer.h)
    endif()
    if("${WAS_RULES}" MATCHES "ACN")
        list(APPEND RULES_OPTIONS -ACN)
        list(APPEND GENERATED_C_SOURCE_PATHS
            ${WAS_GEN_DIR}/asn1crt_encoding_acn.c
            ${WAS_GEN_DIR}/asn1crt_encoding_acn.h)
    endif()

    if(Asn1scc_FOUND)
        # add real compile command
        add_custom_command(OUTPUT ${GENERATED_C_SOURCE_PATHS}
            # first remove everything in the generated file directory to get rid of any old files
            COMMAND ${CMAKE_COMMAND} -E remove_directory ${WAS_GEN_DIR}
            # now recreate the directory
            COMMAND ${CMAKE_COMMAND} -E make_directory ${WAS_GEN_DIR}
            # invoke ASN1C
            COMMAND ${Asn1scc_COMMAND}
                --c-lang
                -o ${WAS_GEN_DIR}
                ${RULES_OPTIONS}
                ${WAS_ASN1_OPTIONS}
                ${WAS_PROTOCOL}
            DEPENDS ${WAS_PROTOCOL}
            WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
            COMMENT "Compiling ASN.1 modules:${ASN1_DISPLAY_MODULES_LIST}")
    else()
        # we can use the existing C files, as long as we have them.
        foreach(GENERATED_C ${GENERATED_C_SOURCE_PATHS})
            # if(EXISTS) can only check absolute paths
            get_filename_component(GENERATED_C_FULLPATH "${GENERATED_C}" ABSOLUTE)
            if(NOT EXISTS "${GENERATED_C_FULLPATH}")
                message(FATAL_ERROR "File ${GENERATED_C} is generated from ASN.1, but asn1scc is not available to generate it.")
            endif()
        endforeach()
    endif()

    # return list of source files
    set(${OUT_VAR} ${GENERATED_C_SOURCE_PATHS} PARENT_SCOPE)

endfunction(wrap_asn1_sources)
