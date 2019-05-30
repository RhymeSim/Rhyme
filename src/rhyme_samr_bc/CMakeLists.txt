cmake_minimum_required( VERSION 3.7 )
enable_language( Fortran )

project( rhyme_samr_bc )

if ( "${CMAKE_Fortran_COMPILER_ID}" STREQUAL "Intel" )
  set( CMAKE_Fortran_FLAGS "-stand f08" )
elseif( "${CMAKE_Fortran_COMPILER_ID}" STREQUAL "GNU" )
  set( CMAKE_Fortran_FLAGS "-std=f2008" )
endif()

set( deps
  rhyme_samr
  rhyme_log
)

set( internal_deps )

set( srcs
  src/rhyme_samr_bc_set_base_grid_boundaries.f90
  src/rhyme_samr_bc.f90
)

set( rhyme_src_dir .. )
include( ${CMAKE_CURRENT_SOURCE_DIR}/../../CMakeGlobal/CMakeLists.txt )
