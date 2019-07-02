project ( rhyme_muscl_hancock_tests )
set( test_subject rhyme_muscl_hancock )

set( CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS}" )

set ( factories
  rhyme_thermo_base
  rhyme_cfl
  rhyme_samr
  rhyme_samr_bc
  rhyme_irs
  rhyme_slope_limiter
  rhyme_chombo
  rhyme_log
)

set( test_deps
  rhyme_assertion
)

set( rhyme_src_dir ../.. )
include( ${CMAKE_CURRENT_SOURCE_DIR}/../../../CMakeGlobal/tests/CMakeLists.txt )

# Configs
include( ${CMAKE_CURRENT_SOURCE_DIR}/${rhyme_src_dir}/../CMakeGlobal/configs/CMakeLists.txt )
