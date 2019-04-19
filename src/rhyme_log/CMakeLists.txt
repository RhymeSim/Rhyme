cmake_minimum_required( VERSION 3.7 )
enable_language( Fortran )

project( rhyme_log )

set( deps )

set( internal_deps
  rhyme_logger_util
)

set( srcs
  src/rhyme_log.f90
)

set( proj_src_dir .. )
include( ${CMAKE_CURRENT_SOURCE_DIR}/../../CMakeGlobal/CMakeLists.txt )
