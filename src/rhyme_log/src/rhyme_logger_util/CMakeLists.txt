cmake_minimum_required( VERSION 3.7 )
enable_language( Fortran )

project( rhyme_logger_util )

set( CMAKE_Fortran_FLAGS "-ffree-line-length-512" )

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

set( proj_src_dir ../../.. )
include( ${CMAKE_CURRENT_SOURCE_DIR}/../../../../CMakeGlobal/CMakeLists.txt )
