project( rhyme_hdf5_util_tests )
set( test_subject rhyme_hdf5_util )

set( factories )

set( test_deps
  rhyme_assertion
)

set( rhyme_src_dir ../../../.. )
include( ${CMAKE_CURRENT_SOURCE_DIR}/${rhyme_src_dir}/../cmake/test_helper.cmake )
