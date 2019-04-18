cmake_minimum_required( VERSION 3.7 )
enable_language( Fortran )

project( rhyme_mh_workspace )

set( deps
  rhyme_hydro_base
  rhyme_samr
  rhyme_log
)

set( internal_deps )

set( srcs
  src/rhyme_mh_workspace_check.f90
  src/rhyme_mh_workspace.f90
)

set( proj_src_dir ../../.. )
include( ${CMAKE_CURRENT_SOURCE_DIR}/../../../../CMakeGlobal/CMakeLists.txt )
