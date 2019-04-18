project ( rhyme_muscl_hancock_tests )
set( test_subject rhyme_muscl_hancock )

set ( factories
  rhyme_samr
  rhyme_samr_bc
  rhyme_chombo
)

set( test_deps )

set( proj_src_dir ../.. )
include( ${CMAKE_CURRENT_SOURCE_DIR}/../../../CMakeGlobal/tests/CMakeLists.txt )
