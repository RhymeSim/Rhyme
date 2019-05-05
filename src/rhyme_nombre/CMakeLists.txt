cmake_minimum_required( VERSION 3.7 )
enable_language( Fortran )

project( rhyme_nombre )

set( deps )

set( internal_deps )

set( srcs
  src/rhyme_nombre_dimension.f90

  src/rhyme_nombre_prefix_mul.f90
  src/rhyme_nombre_prefix.f90

  src/rhyme_nombre_unit_tail.f90
  src/rhyme_nombre_unit_head.f90
  src/rhyme_nombre_unit_clone.f90
  src/rhyme_nombre_unit_print.f90
  src/rhyme_nombre_unit_div.f90
  src/rhyme_nombre_unit_prefix_mul.f90
  src/rhyme_nombre_unit_mul.f90
  src/rhyme_nombre_unit_pow.f90
  src/rhyme_nombre_unit.f90

  src/rhyme_nombre_units_parse.f90
  src/rhyme_nombre_units_parse_single_term.f90
  src/rhyme_nombre_units_is_equal_to.f90
  src/rhyme_nombre_units.f90

  src/rhyme_nombre_print.f90
  src/rhyme_nombre_div.f90
  src/rhyme_nombre_mul.f90
  src/rhyme_nombre_to.f90
  src/rhyme_nombre_new.f90
  src/rhyme_nombre.f90
)

set( proj_src_dir .. )
include( ${CMAKE_CURRENT_SOURCE_DIR}/../../CMakeGlobal/CMakeLists.txt )
