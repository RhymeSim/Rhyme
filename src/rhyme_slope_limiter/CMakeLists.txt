cmake_minimum_required( VERSION 3.12 )
enable_language( Fortran )

project( rhyme_slope_limiter )

if ( "${CMAKE_Fortran_COMPILER_ID}" STREQUAL "Intel" )
  set( CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -stand f08" )
elseif( "${CMAKE_Fortran_COMPILER_ID}" STREQUAL "GNU" )
  set( CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -std=f2008" )
endif()

set( deps
  rhyme_physics
)

set( internal_deps )

set( srcs
  src/rhyme_slope_limiter_superbee.f90
  src/rhyme_slope_limiter_van_albada.f90
  src/rhyme_slope_limiter_minmod.f90
  src/rhyme_slope_limiter_van_leer.f90
  src/rhyme_slope_limiter_xi_r.f90
  src/rhyme_slope_limiter_xi_l.f90
  src/rhyme_slope_limiter_run.f90
  src/rhyme_slope_limiter.f90
)

set( rhyme_src_dir .. )
include( ${CMAKE_CURRENT_SOURCE_DIR}/../../CMakeGlobal/CMakeLists.txt )
