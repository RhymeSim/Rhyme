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
  src/nombre_dimension/rhyme_nombre_dimension_pow.f90
  src/nombre_dimension/rhyme_nombre_dimension_equality.f90
  src/nombre_dimension/rhyme_nombre_dimension.f90


  src/nombre_prefix/rhyme_nombre_prefix_mul.f90
  src/nombre_prefix/rhyme_nombre_prefix_equality.f90
  src/nombre_prefix/rhyme_nombre_prefix.f90


  src/nombre_base_unit/rhyme_nombre_base_unit_parse.f90
  src/nombre_base_unit/rhyme_nombre_base_unit_print.f90
  src/nombre_base_unit/rhyme_nombre_base_unit_update_symbol.f90
  src/nombre_base_unit/rhyme_nombre_base_unit_equality.f90
  src/nombre_base_unit/rhyme_nombre_base_unit_clone.f90
  src/nombre_base_unit/rhyme_nombre_base_unit_new.f90
  src/nombre_base_unit/rhyme_nombre_base_unit.f90


  src/nombre_base_unit_chain/rhyme_nombre_base_unit_chain_div.f90
  src/nombre_base_unit_chain/rhyme_nombre_base_unit_chain_mul.f90
  src/nombre_base_unit_chain/rhyme_nombre_base_unit_chain_pow.f90
  src/nombre_base_unit_chain/rhyme_nombre_base_unit_chain_get_dim.f90
  src/nombre_base_unit_chain/rhyme_nombre_base_unit_chain_print.f90
  src/nombre_base_unit_chain/rhyme_nombre_base_unit_chain_clone.f90
  src/nombre_base_unit_chain/rhyme_nombre_base_unit_chain_tail.f90
  src/nombre_base_unit_chain/rhyme_nombre_base_unit_chain_head.f90
  src/nombre_base_unit_chain/rhyme_nombre_base_unit_chain.f90


  src/nombre_derived_unit/rhyme_nombre_derived_unit_parse.f90
  src/nombre_derived_unit/rhyme_nombre_derived_unit_print.f90
  src/nombre_derived_unit/rhyme_nombre_derived_unit_div.f90
  src/nombre_derived_unit/rhyme_nombre_derived_unit_mul.f90
  src/nombre_derived_unit/rhyme_nombre_derived_unit_equality.f90
  src/nombre_derived_unit/rhyme_nombre_derived_unit_update_symbol.f90
  src/nombre_derived_unit/rhyme_nombre_derived_unit_clone.f90
  src/nombre_derived_unit/rhyme_nombre_derived_unit_new.f90
  src/nombre_derived_unit/rhyme_nombre_derived_unit_init.f90
  src/nombre_derived_unit/rhyme_nombre_derived_unit.f90


  src/nombre_unit/rhyme_nombre_unit_conversion_factor.f90
  src/nombre_unit/rhyme_nombre_unit_parse.f90
  src/nombre_unit/rhyme_nombre_unit_pow.f90
  src/nombre_unit/rhyme_nombre_unit_div.f90
  src/nombre_unit/rhyme_nombre_unit_mul.f90
  src/nombre_unit/rhyme_nombre_unit_print.f90
  src/nombre_unit/rhyme_nombre_unit_clone.f90
  src/nombre_unit/rhyme_nombre_unit_tail.f90
  src/nombre_unit/rhyme_nombre_unit_head.f90
  src/nombre_unit/rhyme_nombre_unit.f90


  src/rhyme_nombre_get_value.f90
  src/rhyme_nombre_print.f90
  src/rhyme_nombre_div.f90
  src/rhyme_nombre_mul.f90
  src/rhyme_nombre_to.f90
  src/rhyme_nombre_new.f90
  src/rhyme_nombre.f90
)

set( rhyme_src_dir .. )
include( ${CMAKE_CURRENT_SOURCE_DIR}/../../CMakeGlobal/CMakeLists.txt )
