cmake_minimum_required( VERSION 3.7 )
enable_language( Fortran )

project( rhyme_hydro_base )

set( deps )

set( internal_deps )

set( srcs
  src/rhyme_hydro_base.f90
)

set( proj_src_dir .. )
include( ${CMAKE_CURRENT_SOURCE_DIR}/../../CMakeGlobal/CMakeLists.txt )
