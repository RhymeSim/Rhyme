project( rhyme_samr_bc_tests )
set( test_subject rhyme_samr_bc )

set( factories
  rhyme_samr
)

set( test_deps )

set( proj_src_dir ../.. )
include( ${CMAKE_CURRENT_SOURCE_DIR}/../../../CMakeGlobal/tests/CMakeLists.txt )
