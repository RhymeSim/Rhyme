cmake_minimum_required( VERSION 3.7 )
enable_language( Fortran )

project( rhyme_samr )

set( deps
  rhyme_hydro_base
)

set( internal_deps )

set( srcs
  src/rhyme_samr.f90
  src/rhyme_samr_init_box.f90
)

set( proj_src_dir .. )
include( ${CMAKE_CURRENT_SOURCE_DIR}/../../CMakeGlobal/CMakeLists.txt )
