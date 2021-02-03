cmake_minimum_required(VERSION 3.12)
enable_language(Fortran)

project(rhyme_sanity_check)

if("${CMAKE_Fortran_COMPILER_ID}" STREQUAL "Intel")
  set(CMAKE_Fortran_FLAGS "-stand f08")
elseif("${CMAKE_Fortran_COMPILER_ID}" STREQUAL "GNU")
  set(CMAKE_Fortran_FLAGS "-std=f2008")
endif()

set(deps rhyme_nombre rhyme_units rhyme_thermo_base rhyme_samr rhyme_logger)

set(internal_deps)

set(srcs
    src/rhyme_sanity_check_init.f90 src/rhyme_sanity_check_fill.f90
    src/rhyme_sanity_check_print.f90 src/rhyme_sanity_check_perform.f90
    src/rhyme_sanity_check.f90)

set(rhyme_src_dir ..)
include(${CMAKE_CURRENT_SOURCE_DIR}/${rhyme_src_dir}/../cmake/helper.cmake)

# Configs
include(${CMAKE_CURRENT_SOURCE_DIR}/${rhyme_src_dir}/../config.cmake)
