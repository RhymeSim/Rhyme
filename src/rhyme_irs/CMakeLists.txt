cmake_minimum_required(VERSION 3.12)
enable_language(Fortran)

project(rhyme_irs)

if("${CMAKE_Fortran_COMPILER_ID}" STREQUAL "Intel")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -stand f08")
elseif("${CMAKE_Fortran_COMPILER_ID}" STREQUAL "GNU")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -std=f2008")
endif()

set(deps rhyme_units rhyme_hydro_base rhyme_thermo_base rhyme_logger)

set(internal_deps)

set(srcs
    src/rhyme_irs_guess_p_star.f90 src/rhyme_irs_nonlinear_wave_function.f90
    src/rhyme_irs_exec.f90 src/rhyme_irs_init.f90 src/rhyme_irs.f90)

set(rhyme_src_dir ..)
include(${CMAKE_CURRENT_SOURCE_DIR}/${rhyme_src_dir}/../cmake/helper.cmake)

# Configs
include(${CMAKE_CURRENT_SOURCE_DIR}/${rhyme_src_dir}/../config.cmake)
