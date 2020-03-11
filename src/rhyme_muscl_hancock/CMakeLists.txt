cmake_minimum_required(VERSION 3.12)
enable_language(Fortran)

project(rhyme_muscl_hancock)

if("${CMAKE_Fortran_COMPILER_ID}" STREQUAL "Intel")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -stand f08")
elseif("${CMAKE_Fortran_COMPILER_ID}" STREQUAL "GNU")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -std=f2008")
endif()

set(deps rhyme_physics rhyme_thermo_base rhyme_irs rhyme_slope_limiter
         rhyme_logger)

set(internal_deps rhyme_mh_workspace)

set(srcs
    src/rhyme_muscl_hancock_half_step_extrapolation.f90
    src/rhyme_muscl_hancock_solve_memory_intensive.f90
    src/rhyme_muscl_hancock_solve_cpu_intensive.f90
    src/rhyme_muscl_hancock_solve.f90
    src/rhyme_muscl_hancock_init.f90
    src/rhyme_muscl_hancock.f90)

set(rhyme_src_dir ..)
include(${CMAKE_CURRENT_SOURCE_DIR}/${rhyme_src_dir}/../cmake/helper.cmake)

# Configs
include(${CMAKE_CURRENT_SOURCE_DIR}/${rhyme_src_dir}/../config.cmake)
