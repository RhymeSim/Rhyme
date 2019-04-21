cmake_minimum_required( VERSION 3.7 )
enable_language( Fortran )

project( rhyme_assertion )

set( deps
  rhyme_log
)

set( internal_deps )

set( srcs
  src/rhyme_assertion_failed.f90
  src/rhyme_assertion_passed.f90
  src/rhyme_assertion_add_test_message.f90
  src/rhyme_assertion_not_to_be_array_scalar.f90
  src/rhyme_assertion_not_to_be_array.f90
  src/rhyme_assertion_not_to_be.f90
  src/rhyme_assertion_to_be_array_2d.f90
  src/rhyme_assertion_to_be_array_scalar.f90
  src/rhyme_assertion_to_be_array.f90
  src/rhyme_assertion_to_be.f90
  src/rhyme_assertion_expect.f90
  src/rhyme_assertion_describe.f90
  src/rhyme_assertion_reset.f90
  src/rhyme_assertion.f90
)

set( proj_src_dir .. )
include( ${CMAKE_CURRENT_SOURCE_DIR}/../../CMakeGlobal/CMakeLists.txt )
