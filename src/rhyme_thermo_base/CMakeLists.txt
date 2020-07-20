cmake_minimum_required(VERSION 3.12)
enable_language(Fortran)

project(rhyme_thermo_base)

if("${CMAKE_Fortran_COMPILER_ID}" STREQUAL "Intel")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -stand f08")
elseif("${CMAKE_Fortran_COMPILER_ID}" STREQUAL "GNU")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -std=f2008")
endif()

set(deps rhyme_units rhyme_ideal_gas rhyme_logger)

set(internal_deps)

set(srcs
    src/rhyme_thermo_base_specific_internal_energy.f90
    src/rhyme_thermo_base_primitive_vars_to_conserved.f90
    src/rhyme_thermo_base_primitive_to_conserved.f90
    src/rhyme_thermo_base_conserved_to_primitive.f90
    src/rhyme_thermo_base_flux.f90
    src/rhyme_thermo_base_pressure.f90
    src/rhyme_thermo_base_speed_of_sound.f90
    src/rhyme_thermo_base_temperature_per_mu.f90
    src/rhyme_thermo_base_get_gamma.f90
    src/rhyme_thermo_base.f90)

set(rhyme_src_dir ../)
include(${CMAKE_CURRENT_SOURCE_DIR}/${rhyme_src_dir}/../cmake/helper.cmake)

# Configs
include(${CMAKE_CURRENT_SOURCE_DIR}/${rhyme_src_dir}/../config.cmake)
