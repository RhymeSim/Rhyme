cmake_minimum_required( VERSION 3.12 )
enable_language( Fortran )

project( rhyme_logger_util )

if ( "${CMAKE_Fortran_COMPILER_ID}" STREQUAL "Intel" )
  set( CMAKE_Fortran_FLAGS "-stand f08 -extend-source 512" )
elseif( "${CMAKE_Fortran_COMPILER_ID}" STREQUAL "GNU" )
  set( CMAKE_Fortran_FLAGS "-std=f2008 -ffree-line-length-512" )
endif()

set( deps
  rhyme_string
)

set( internal_deps )

set( srcs
  src/rhyme_logger_util.f90
  src/rhyme_logger_util_log.f90
  src/rhyme_logger_util_warn.f90
  src/rhyme_logger_util_err.f90
  src/rhyme_logger_util_task.f90
  src/rhyme_logger_util_io.f90
  src/rhyme_logger_util_logo.f90
  src/rhyme_logger_util_time.f90
  src/rhyme_logger_util_section.f90
)

set( rhyme_src_dir ../../.. )
include( ${CMAKE_CURRENT_SOURCE_DIR}/../../../../CMakeGlobal/CMakeLists.txt )
