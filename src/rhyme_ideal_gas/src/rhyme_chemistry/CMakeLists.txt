cmake_minimum_required( VERSION 3.7 )
enable_language( Fortran )

project( rhyme_chemistry )

set( deps
  rhyme_nombre
  rhyme_units
  rhyme_log
)

set( internal_deps )

set( srcs
  src/rhyme_chemistry_init.f90
  src/rhyme_chemistry_mu.f90
  src/rhyme_chemistry_one_over_mu.f90
  src/rhyme_chemistry.f90
)

set( proj_src_dir ../../.. )
include( ${CMAKE_CURRENT_SOURCE_DIR}/../../../../CMakeGlobal/CMakeLists.txt )
