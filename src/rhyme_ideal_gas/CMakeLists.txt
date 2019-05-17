cmake_minimum_required( VERSION 3.7 )
enable_language( Fortran )

project( rhyme_ideal_gas )

if ( "${CMAKE_Fortran_COMPILER_ID}" STREQUAL "Intel" )
  set( CMAKE_Fortran_FLAGS "-stand f08" )
elseif( "${CMAKE_Fortran_COMPILER_ID}" STREQUAL "GNU" )
  set( CMAKE_Fortran_FLAGS "-std=f2008" )
endif()

set( deps
  rhyme_units
  rhyme_hydro_base
  rhyme_log
)

set( internal_deps
  rhyme_chemistry
  rhyme_thermo_base
)

set( srcs
  src/rhyme_ideal_gas_init.f90
  src/rhyme_ideal_gas.f90
)

set( proj_src_dir .. )
include( ${CMAKE_CURRENT_SOURCE_DIR}/../../CMakeGlobal/CMakeLists.txt )
