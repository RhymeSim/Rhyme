project( rhyme_cfl_tests )
set( test_subject rhyme_cfl )

set( factories
  rhyme_samr
  rhyme_ideal_gas
)

set( test_deps
  rhyme_assertion
)

set( proj_src_dir ../.. )
include( ${CMAKE_CURRENT_SOURCE_DIR}/../../../CMakeGlobal/tests/CMakeLists.txt )
