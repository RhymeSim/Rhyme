cmake_minimum_required(VERSION 3.12)
enable_language(Fortran)

project(rhyme_samr_bc)

if("${CMAKE_Fortran_COMPILER_ID}" STREQUAL "Intel")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -stand f08")
elseif("${CMAKE_Fortran_COMPILER_ID}" STREQUAL "GNU")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -std=f2008")
endif()

set(deps rhyme_units rhyme_thermo_base rhyme_samr rhyme_logger)

set(internal_deps)

set(srcs
    src/rhyme_samr_bc_set_front_boundary.f90
    src/rhyme_samr_bc_set_back_boundary.f90
    src/rhyme_samr_bc_set_top_boundary.f90
    src/rhyme_samr_bc_set_bottom_boundary.f90
    src/rhyme_samr_bc_set_right_boundary.f90
    src/rhyme_samr_bc_set_left_boundary.f90
    src/rhyme_samr_bc_set_boundaries.f90
    src/rhyme_samr_bc_init.f90
    src/rhyme_samr_bc.f90)

set(rhyme_src_dir ..)
include(${CMAKE_CURRENT_SOURCE_DIR}/${rhyme_src_dir}/../cmake/helper.cmake)

# Configs
include(${CMAKE_CURRENT_SOURCE_DIR}/${rhyme_src_dir}/../config.cmake)
