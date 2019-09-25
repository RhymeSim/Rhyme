cmake_minimum_required( VERSION 3.12 )
enable_language( Fortran )

project( rhyme_spectrum )

if ( "${CMAKE_Fortran_COMPILER_ID}" STREQUAL "Intel" )
  set( CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -stand f08" )
elseif( "${CMAKE_Fortran_COMPILER_ID}" STREQUAL "GNU" )
  set( CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -std=f2008" )
endif()

set( deps
  rhyme_logger
  rhyme_nombre
)

set( internal_deps )

set( srcs
  src/rhyme_spectrum_dispatch_logarithmic_region.f90
  src/rhyme_spectrum_dispatch_linear_region.f90
  src/rhyme_spectrum_dispatch_regions.f90
  src/rhyme_spectrum_new_region.f90
  src/rhyme_spectrum.f90
)

set( rhyme_src_dir .. )
include( ${CMAKE_CURRENT_SOURCE_DIR}/../../CMakeGlobal/CMakeLists.txt )
