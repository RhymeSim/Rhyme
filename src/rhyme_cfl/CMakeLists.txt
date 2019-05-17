cmake_minimum_required( VERSION 3.7 )
enable_language( Fortran )

project( rhyme_cfl )

if ( "${CMAKE_Fortran_COMPILER_ID}" STREQUAL "Intel" )
  set( CMAKE_Fortran_FLAGS "-stand f08" )
elseif( "${CMAKE_Fortran_COMPILER_ID}" STREQUAL "GNU" )
  set( CMAKE_Fortran_FLAGS "-std=f2008" )
endif()

set( deps
  rhyme_samr
  rhyme_ideal_gas
  rhyme_log
)

set( internal_deps )

set( srcs
  src/rhyme_cfl_time_step.f90
  src/rhyme_cfl.f90
)

set( proj_src_dir .. )
include( ${CMAKE_CURRENT_SOURCE_DIR}/../../CMakeGlobal/CMakeLists.txt )
