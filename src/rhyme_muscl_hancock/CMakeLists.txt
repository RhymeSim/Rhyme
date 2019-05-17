cmake_minimum_required( VERSION 3.7 )
enable_language( Fortran )

project( rhyme_muscl_hancock )

if ( "${CMAKE_Fortran_COMPILER_ID}" STREQUAL "Intel" )
  set( CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -stand f08" )
elseif( "${CMAKE_Fortran_COMPILER_ID}" STREQUAL "GNU" )
  set( CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -std=f2008" )
endif()

set( deps
  rhyme_ideal_gas
  rhyme_slope_limiter
  rhyme_irs
  rhyme_log
)

set( internal_deps
  rhyme_mh_workspace
)

set( srcs
  src/rhyme_muscl_hancock_solve_memory_intensive.f90
  src/rhyme_muscl_hancock_solve_cpu_intensive.f90
  src/rhyme_muscl_hancock_solve.f90
  src/rhyme_muscl_hancock_init.f90
  src/rhyme_muscl_hancock.f90
)

set( proj_src_dir .. )
include( ${CMAKE_CURRENT_SOURCE_DIR}/../../CMakeGlobal/CMakeLists.txt )
