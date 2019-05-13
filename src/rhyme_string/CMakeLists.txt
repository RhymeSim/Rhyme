cmake_minimum_required( VERSION 3.7 )
enable_language( Fortran )

project( rhyme_string )

set( deps )
set( internal_deps )

set( srcs
  src/rhyme_string_get_filename.f90
  src/rhyme_string_is_nan.f90
  src/rhyme_string_to_string.f90
  src/rhyme_string.f90
)

set( proj_src_dir .. )
include( ${CMAKE_CURRENT_SOURCE_DIR}/../../CMakeGlobal/CMakeLists.txt )
