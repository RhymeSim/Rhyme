project( rhyme_ideal_gas_tests )
set( test_subject rhyme_ideal_gas )

set( factories
  rhyme_hydro_base
)

set( test_deps
  rhyme_assertion
)

set( rhyme_src_dir ../.. )
include( ${CMAKE_CURRENT_SOURCE_DIR}/../../../CMakeGlobal/tests/CMakeLists.txt )
