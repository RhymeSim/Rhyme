project( rhyme_samr_tests )
set( test_subject rhyme_samr )

set( factories )

set( test_deps
  rhyme_assertion
)

set( proj_src_dir ../.. )
include( ${CMAKE_CURRENT_SOURCE_DIR}/../../../CMakeGlobal/tests/CMakeLists.txt )

if_not_defined_define( "NDIM" 3 )
if_not_defined_define( "NCMP" 5 )
