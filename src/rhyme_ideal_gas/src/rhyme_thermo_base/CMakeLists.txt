cmake_minimum_required( VERSION 3.7 )
enable_language( Fortran )

project( rhyme_thermo_base )

set( deps
  rhyme_units
  rhyme_log
)

set( internal_deps )

set( srcs
  src/rhyme_thermo_base_init.f90
  src/rhyme_thermo_base.f90
)

set( proj_src_dir ../../.. )
include( ${CMAKE_CURRENT_SOURCE_DIR}/../../../../CMakeGlobal/CMakeLists.txt )
