cmake_minimum_required( VERSION 3.7 )
enable_language( Fortran )

project( rhyme_units )

set( deps
  rhyme_nombre
)

set( internal_deps )

set( srcs
  src/rhyme_units.f90
)

set( proj_src_dir .. )
include( ${CMAKE_CURRENT_SOURCE_DIR}/../../CMakeGlobal/CMakeLists.txt )
