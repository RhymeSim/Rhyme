cmake_minimum_required( VERSION 3.12 )
enable_language( Fortran )

project( rhyme_samr )

if ( "${CMAKE_Fortran_COMPILER_ID}" STREQUAL "Intel" )
  set( CMAKE_Fortran_FLAGS "-stand f08" )
elseif( "${CMAKE_Fortran_COMPILER_ID}" STREQUAL "GNU" )
  set( CMAKE_Fortran_FLAGS "-std=f2008" )
endif()

set( deps
  rhyme_hydro_base
)

set( internal_deps )

set( srcs
  src/rhyme_samr.f90
  src/rhyme_samr_init_box.f90
)

set( proj_src_dir .. )
include( ${CMAKE_CURRENT_SOURCE_DIR}/../../CMakeGlobal/CMakeLists.txt )

if_not_defined_define( "NDIM" 3 )
if_not_defined_define( "NCMP" 5 )
