cmake_minimum_required( VERSION 3.7 )
enable_language( Fortran )

project( rhyme_chombo )

set( deps
  rhyme_samr
  rhyme_log
)

set( internal_deps
  rhyme_hdf5_util
)

set( srcs
  src/rhyme_chombo.f90
)

set( proj_src_dir .. )
include( ${CMAKE_CURRENT_SOURCE_DIR}/../../CMakeGlobal/CMakeLists.txt )
