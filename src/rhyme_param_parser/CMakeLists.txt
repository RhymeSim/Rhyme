cmake_minimum_required( VERSION 3.7 )
enable_language( Fortran )

project( rhyme_param_parser )

set( deps
  rhyme_log
  rhyme_units
  rhyme_string
  rhyme_samr_bc
  rhyme_cfl
  rhyme_ideal_gas
  rhyme_drawing
  rhyme_irs
  rhyme_slope_limiter
  rhyme_muscl_hancock
  rhyme_chombo
  rhyme_initial_condition
)

set( internal_deps )

set( srcs
  src/rhyme_param_parser_add_hint.f90
  src/rhyme_param_parser_add_occur.f90
  src/rhyme_param_parser_occurences.f90
  src/rhyme_param_parser_read_array.f90
  src/rhyme_param_parser_read.f90
  src/rhyme_param_parser_new_term.f90
  src/rhyme_param_parser_add_switch.f90
  src/rhyme_param_parser_init.f90
  src/rhyme_param_parser_load_params.f90
  src/rhyme_param_parser.f90
)

set( proj_src_dir .. )
include( ${CMAKE_CURRENT_SOURCE_DIR}/../../CMakeGlobal/CMakeLists.txt )
