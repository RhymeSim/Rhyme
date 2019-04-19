cmake_minimum_required( VERSION 3.7 )
enable_language( Fortran )

project( rhyme_ideal_gas )

set( deps
  rhyme_hydro_base
  rhyme_log
)

set( internal_deps
  rhyme_chemistry
  rhyme_thermo_base
)

set( srcs
  src/rhyme_ideal_gas.f90
)

set( proj_src_dir .. )
include( ${CMAKE_CURRENT_SOURCE_DIR}/../../CMakeGlobal/CMakeLists.txt )
