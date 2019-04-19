cmake_minimum_required( VERSION 3.7 )
enable_language( Fortran )

project( rhyme_chemistry )

set( deps
  rhyme_nombre
  rhyme_log
)

set( internal_deps )

set( srcs
  src/rhyme_chemistry.f90
)

set( proj_src_dir ../../.. )
include( ${CMAKE_CURRENT_SOURCE_DIR}/../../../../CMakeGlobal/CMakeLists.txt )
