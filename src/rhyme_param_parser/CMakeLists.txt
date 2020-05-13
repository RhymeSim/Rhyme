cmake_minimum_required(VERSION 3.12)
enable_language(Fortran)

project(rhyme_param_parser)

if("${CMAKE_Fortran_COMPILER_ID}" STREQUAL "Intel")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -stand f08")
elseif("${CMAKE_Fortran_COMPILER_ID}" STREQUAL "GNU")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -std=f2008")
endif()

set(deps
    rhyme_chemistry
    rhyme_physics
    rhyme_string
    rhyme_samr_bc
    rhyme_cfl
    rhyme_thermo_base
    rhyme_uv_background
    rhyme_ionisation_equilibrium
    rhyme_drawing
    rhyme_irs
    rhyme_slope_limiter
    rhyme_muscl_hancock
    rhyme_chombo
    rhyme_initial_condition
    rhyme_logger)

set(internal_deps)

set(srcs
    src/rhyme_param_parser_add_hint.f90
    src/rhyme_param_parser_add_occur.f90
    src/rhyme_param_parser_occurences.f90
    src/rhyme_param_parser_read_array.f90
    src/rhyme_param_parser_read_single.f90
    src/rhyme_param_parser_new_term.f90
    src/rhyme_param_parser_add_switch.f90
    src/rhyme_param_parser_load_params.f90
    src/rhyme_param_parser.f90)

set(rhyme_src_dir ..)
include(${CMAKE_CURRENT_SOURCE_DIR}/${rhyme_src_dir}/../cmake/helper.cmake)

# Configs
include(${CMAKE_CURRENT_SOURCE_DIR}/${rhyme_src_dir}/../config.cmake)
