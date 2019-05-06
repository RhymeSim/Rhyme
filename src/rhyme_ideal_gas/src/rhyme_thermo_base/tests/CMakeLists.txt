project( rhyme_thermo_base_tests )
set( test_subject rhyme_thermo_base )

set( factories
  rhyme_units
)

set( test_deps
  rhyme_assertion
)

set( proj_src_dir ../../../.. )
include( ${CMAKE_CURRENT_SOURCE_DIR}/../../../../../CMakeGlobal/tests/CMakeLists.txt )
