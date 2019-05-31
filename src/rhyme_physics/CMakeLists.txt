cmake_minimum_required( VERSION 3.12 )
enable_language( Fortran )

project( rhyme_physics )

if ( "${CMAKE_Fortran_COMPILER_ID}" STREQUAL "Intel" )
  set( CMAKE_Fortran_FLAGS "-stand f08" )
elseif( "${CMAKE_Fortran_COMPILER_ID}" STREQUAL "GNU" )
  set( CMAKE_Fortran_FLAGS "-std=f2008" )
endif()

set( deps
  rhyme_nombre
  rhyme_log
)

set( internal_deps )

set( srcs
  src/rhyme_physics_init.f90
  src/rhyme_physics.f90
)

set( rhyme_src_dir .. )
include( ${CMAKE_CURRENT_SOURCE_DIR}/../../CMakeGlobal/CMakeLists.txt )

if_not_defined_define( HYDRO_SOLVER )
if_not_defined_define( RT_SOLVER )
if_not_defined_define( "NDIM" 3 )
if_not_defined_define( "NCMP" 5 )
if_not_defined_define( "NSPE" 3 )
