cmake_minimum_required(VERSION 3.12)
enable_language(Fortran)

project(rhyme_samr)

if("${CMAKE_Fortran_COMPILER_ID}" STREQUAL "Intel")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -stand f08")
elseif("${CMAKE_Fortran_COMPILER_ID}" STREQUAL "GNU")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -std=f2008")
endif()

set(deps rhyme_hydro_base)

set(internal_deps)

set(srcs
    src/rhyme_samr.f90 src/rhyme_samr_weighted_average_of_distances.f90
    src/rhyme_samr_calc_total_energy.f90 src/rhyme_samr_calc_total_mass.f90
    src/rhyme_samr_init_box.f90)

set(rhyme_src_dir ..)
include(${CMAKE_CURRENT_SOURCE_DIR}/${rhyme_src_dir}/../cmake/helper.cmake)

# Configs
include(${CMAKE_CURRENT_SOURCE_DIR}/${rhyme_src_dir}/../config.cmake)
