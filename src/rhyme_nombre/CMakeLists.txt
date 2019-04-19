cmake_minimum_required( VERSION 3.7 )
enable_language( Fortran )

project( rhyme_nombre )

set( deps )

set( internal_deps )

set( srcs
  src/rhyme_dimension.f90
  src/rhyme_nombre.f90
  src/rhyme_prefix.f90
  src/rhyme_unit.f90
  src/rhyme_units.f90
)

set( proj_src_dir .. )
include( ${CMAKE_CURRENT_SOURCE_DIR}/../../CMakeGlobal/CMakeLists.txt )
