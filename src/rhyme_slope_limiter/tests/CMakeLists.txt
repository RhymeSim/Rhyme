project( rhyme_slope_limiter_tests )
set( test_subject rhyme_slope_limiter )

set( factories
  rhyme_hydro_base
)

set( test_deps
  rhyme_assertion
)

set( rhyme_src_dir ../.. )
include( ${CMAKE_CURRENT_SOURCE_DIR}/../../../CMakeGlobal/tests/CMakeLists.txt )

# Configs
include( ${CMAKE_CURRENT_SOURCE_DIR}/${rhyme_src_dir}/../CMakeGlobal/configs/CMakeLists.txt )
