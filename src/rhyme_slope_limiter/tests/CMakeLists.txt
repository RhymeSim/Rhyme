project( rhyme_slope_limiter_tests )
set( test_subject rhyme_slope_limiter )

set( factories
  rhyme_hydro_base
  rhyme_ideal_gas
)

set( test_deps
  rhyme_assertion
)

set( rhyme_src_dir ../.. )
include( ${CMAKE_CURRENT_SOURCE_DIR}/../../../CMakeGlobal/tests/CMakeLists.txt )
