cmake_minimum_required( VERSION 3.7 )
enable_language( Fortran )

project( rhyme_samr_bc )

set( deps
  rhyme_samr
  rhyme_log
)

set( internal_deps )

set( srcs
  src/rhyme_samr_bc_set_base_grid_boundaries.f90
  src/rhyme_samr_bc.f90
)

set( proj_src_dir .. )
include( ${CMAKE_CURRENT_SOURCE_DIR}/../../CMakeGlobal/CMakeLists.txt )
