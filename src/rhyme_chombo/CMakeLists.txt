cmake_minimum_required( VERSION 3.7 )
enable_language( Fortran )

project( rhyme_chombo )

if ( "${CMAKE_Fortran_COMPILER_ID}" STREQUAL "Intel" )
  set( CMAKE_Fortran_FLAGS "-stand f08" )
elseif( "${CMAKE_Fortran_COMPILER_ID}" STREQUAL "GNU" )
  set( CMAKE_Fortran_FLAGS "-std=f2008" )
endif()

set( deps
  rhyme_samr
  rhyme_log
)

set( internal_deps
  rhyme_hdf5_util
)

set( srcs
  src/rhyme_chombo.f90
)

set( proj_src_dir .. )
include( ${CMAKE_CURRENT_SOURCE_DIR}/../../CMakeGlobal/CMakeLists.txt )
