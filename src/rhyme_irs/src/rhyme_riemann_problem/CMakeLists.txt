cmake_minimum_required( VERSION 3.5 )
enable_language( Fortran )

project( rhyme_riemann_problem )

set( deps )

set( internal_deps )

set( srcs
  src/rhyme_riemann_problem.f90
)

set( proj_src_dir ../../.. )
include( ${CMAKE_CURRENT_SOURCE_DIR}/../../../../CMakeGlobal/CMakeLists.txt )
