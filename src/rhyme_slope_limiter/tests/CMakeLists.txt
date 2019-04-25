project( rhyme_slope_limiter_tests )
set( test_subject rhyme_slope_limiter )

set( factories
  rhyme_hydro_base
)

set( test_deps
  rhyme_assertion
)

set( proj_src_dir ../.. )
include( ${CMAKE_CURRENT_SOURCE_DIR}/../../../CMakeGlobal/tests/CMakeLists.txt )
