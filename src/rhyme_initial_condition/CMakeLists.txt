cmake_minimum_required(VERSION 3.12)
enable_language(Fortran)

project(rhyme_initial_condition)

if("${CMAKE_Fortran_COMPILER_ID}" STREQUAL "Intel")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -stand f08")
elseif("${CMAKE_Fortran_COMPILER_ID}" STREQUAL "GNU")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -std=f2008")
endif()

set(deps rhyme_units rhyme_samr rhyme_chombo rhyme_logger)

set(internal_deps)

set(srcs
    src/rhyme_initial_condition_load_radamesh.f90
    src/rhyme_initial_condition_load_rhyme.f90
    src/rhyme_initial_condition_load_headers.f90
    src/rhyme_initial_condition_load_snapshot.f90
    src/rhyme_initial_condition_init_simple.f90
    src/rhyme_initial_condition_init.f90
    src/rhyme_initial_condition.f90)

set(rhyme_src_dir ..)
include(${CMAKE_CURRENT_SOURCE_DIR}/${rhyme_src_dir}/../cmake/helper.cmake)

# Configs
include(${CMAKE_CURRENT_SOURCE_DIR}/${rhyme_src_dir}/../config.cmake)
