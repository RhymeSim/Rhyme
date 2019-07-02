cmake_minimum_required( VERSION 3.12 )
enable_language( Fortran )

project( rhyme_log )

if ( "${CMAKE_Fortran_COMPILER_ID}" STREQUAL "Intel" )
  set( CMAKE_Fortran_FLAGS "-stand f08" )
elseif( "${CMAKE_Fortran_COMPILER_ID}" STREQUAL "GNU" )
  set( CMAKE_Fortran_FLAGS "-std=f2008" )
endif()

set( deps )

set( internal_deps
  rhyme_logger_util
)

set( srcs
  src/rhyme_log.f90
)

set( rhyme_src_dir .. )
include( ${CMAKE_CURRENT_SOURCE_DIR}/../../CMakeGlobal/CMakeLists.txt )
