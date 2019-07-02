cmake_minimum_required( VERSION 3.12 )
enable_language( Fortran )

project( rhyme_hydro_base )

if ( "${CMAKE_Fortran_COMPILER_ID}" STREQUAL "Intel" )
  set( CMAKE_Fortran_FLAGS "-stand f08" )
elseif( "${CMAKE_Fortran_COMPILER_ID}" STREQUAL "GNU" )
  set( CMAKE_Fortran_FLAGS "-std=f2008" )
endif()

set( deps
  rhyme_physics
)

set( internal_deps )

set( srcs
  src/rhyme_hydro_base_specific_internal_energy.f90
  src/rhyme_hydro_base_specific_kinetic_energy.f90
  src/rhyme_hydro_base_primitive_to_conserved.f90
  src/rhyme_hydro_base.f90
)

set( rhyme_src_dir .. )
include( ${CMAKE_CURRENT_SOURCE_DIR}/${rhyme_src_dir}/../CMakeGlobal/CMakeLists.txt )

# Configs
include( ${CMAKE_CURRENT_SOURCE_DIR}/${rhyme_src_dir}/../CMakeGlobal/configs/CMakeLists.txt )
