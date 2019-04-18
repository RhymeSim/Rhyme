cmake_minimum_required( VERSION 3.7 )
enable_language( Fortran )

project( rhyme_initial_condition )

set( deps
  rhyme_samr
  rhyme_chombo
  rhyme_ideal_gas
  rhyme_log
)

set( internal_deps )

set( srcs
  src/rhyme_initial_condition.f90
)

set( proj_src_dir .. )
include( ${CMAKE_CURRENT_SOURCE_DIR}/../../CMakeGlobal/CMakeLists.txt )
