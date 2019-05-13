project ( rhyme_muscl_hancock_tests )
set( test_subject rhyme_muscl_hancock )

set( CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -O3" )

set ( factories
  rhyme_chombo
  rhyme_cfl
  rhyme_ideal_gas
  rhyme_hydro_base
  rhyme_samr
  rhyme_samr_bc
  rhyme_irs
  rhyme_slope_limiter
  rhyme_log
)

set( test_deps
  rhyme_assertion
)

set( proj_src_dir ../.. )
include( ${CMAKE_CURRENT_SOURCE_DIR}/../../../CMakeGlobal/tests/CMakeLists.txt )
