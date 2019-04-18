cmake_minimum_required( VERSION 3.7 )
enable_language( Fortran )

project( radamesh_spectrum )

set( deps
  rhyme_log
)

set( internal_deps )

set( srcs
  src/radamesh_spectrum.f90
)

set( proj_src_dir .. )
include( ${CMAKE_CURRENT_SOURCE_DIR}/../../CMakeGlobal/CMakeLists.txt )
