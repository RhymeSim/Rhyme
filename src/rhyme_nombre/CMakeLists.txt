cmake_minimum_required( VERSION 3.12 )
enable_language( Fortran )

project( rhyme_nombre )

if ( "${CMAKE_Fortran_COMPILER_ID}" STREQUAL "Intel" )
  set( CMAKE_Fortran_FLAGS "-stand f08" )
elseif( "${CMAKE_Fortran_COMPILER_ID}" STREQUAL "GNU" )
  set( CMAKE_Fortran_FLAGS "-std=f2008" )
endif()

set( deps )

set( internal_deps )

set( srcs
  src/nombre_dimension/rhyme_nombre_dimension_equality.f90
  src/nombre_dimension/rhyme_nombre_dimension.f90


  src/nombre_prefix/rhyme_nombre_prefix_mul.f90
  src/nombre_prefix/rhyme_nombre_prefix_equality.f90
  src/nombre_prefix/rhyme_nombre_prefix.f90


  src/nombre_base_unit/rhyme_nombre_base_unit_print.f90
  src/nombre_base_unit/rhyme_nombre_base_unit_update_symbol.f90
  src/nombre_base_unit/rhyme_nombre_base_unit_pow.f90
  src/nombre_base_unit/rhyme_nombre_base_unit_mul.f90
  src/nombre_base_unit/rhyme_nombre_base_unit_clone.f90
  src/nombre_base_unit/rhyme_nombre_base_unit_equality.f90
  src/nombre_base_unit/rhyme_nombre_base_unit_head.f90
  src/nombre_base_unit/rhyme_nombre_base_unit_tail.f90
  src/nombre_base_unit/rhyme_nombre_base_unit.f90


  src/nombre_derived_unit/rhyme_nombre_derived_unit_chain_tail.f90
  src/nombre_derived_unit/rhyme_nombre_derived_unit_chain_head.f90

  src/nombre_derived_unit/rhyme_nombre_derived_unit_print.f90
  src/nombre_derived_unit/rhyme_nombre_derived_unit_div.f90
  src/nombre_derived_unit/rhyme_nombre_derived_unit_pow.f90
  src/nombre_derived_unit/rhyme_nombre_derived_unit_mul.f90
  src/nombre_derived_unit/rhyme_nombre_derived_unit_equality.f90
  src/nombre_derived_unit/rhyme_nombre_derived_unit_clone.f90
  src/nombre_derived_unit/rhyme_nombre_derived_unit_update_symbol.f90
  src/nombre_derived_unit/rhyme_nombre_derived_unit_get_dim.f90
  src/nombre_derived_unit/rhyme_nombre_derived_unit_new.f90
  src/nombre_derived_unit/rhyme_nombre_derived_unit.f90

  src/nombre_parse/rhyme_nombre_parse_find_derived_unit.f90
  src/nombre_parse/rhyme_nombre_parse_find_unit.f90
  src/nombre_parse/rhyme_nombre_parse_close_par_loc.f90
  src/nombre_parse/rhyme_nombre_parse_tokenize.f90
  src/nombre_parse/rhyme_nombre_parse.f90

#   src/nombre_units/rhyme_nombre_units_simplify.f90
#   src/nombre_units/rhyme_nombre_units_clone.f90
#   src/nombre_units/rhyme_nombre_units_parse.f90
#   src/nombre_units/rhyme_nombre_units_parse_single_term.f90
#   src/nombre_units/rhyme_nombre_units_is_equal_to.f90
#   src/nombre_units/rhyme_nombre_units_tail.f90
#   src/nombre_units/rhyme_nombre_units_head.f90
#   src/nombre_units/rhyme_nombre_units_print.f90
#   src/nombre_units/rhyme_nombre_units_print_dim.f90
#   src/nombre_units/rhyme_nombre_units_get_dim.f90
#   src/nombre_units/rhyme_nombre_units_conv_factor.f90
#   src/nombre_units/rhyme_nombre_units_div.f90
#   src/nombre_units/rhyme_nombre_units_mul.f90
#   src/nombre_units/rhyme_nombre_units_pow.f90
#   src/nombre_units/rhyme_nombre_units.f90
#
#   src/rhyme_nombre_get_value.f90
#   src/rhyme_nombre_print.f90
#   src/rhyme_nombre_div.f90
#   src/rhyme_nombre_mul.f90
#   src/rhyme_nombre_to.f90
#   src/rhyme_nombre_new.f90
#   src/rhyme_nombre.f90
)

set( rhyme_src_dir .. )
include( ${CMAKE_CURRENT_SOURCE_DIR}/../../CMakeGlobal/CMakeLists.txt )
