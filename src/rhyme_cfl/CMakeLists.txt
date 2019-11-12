cmake_minimum_required( VERSION 3.12 )
enable_language( Fortran )

project( rhyme_cfl )

if ( "${CMAKE_Fortran_COMPILER_ID}" STREQUAL "Intel" )
  set( CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -stand f08" )
elseif( "${CMAKE_Fortran_COMPILER_ID}" STREQUAL "GNU" )
  set( CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -std=f2008" )
endif()

set( deps
  rhyme_samr
  rhyme_thermo_base
)

set( internal_deps )

set( srcs
  src/rhyme_cfl_time_step.f90
  src/rhyme_cfl.f90
)

set( rhyme_src_dir .. )
include( ${CMAKE_CURRENT_SOURCE_DIR}/${rhyme_src_dir}/../cmake/helper.cmake )

# Configs
include( ${CMAKE_CURRENT_SOURCE_DIR}/${rhyme_src_dir}/../config.cmake )
