project( rhyme_initial_condition_tests )
set( test_subject rhyme_initial_condition )

set( factories
  rhyme_samr
  rhyme_units
)

set( test_deps
  rhyme_assertion
)

configure_file(
  "./assets/r2c_2d_sample.hdf5"
  "r2c_2d_sample.hdf5"
  COPYONLY
)

set( rhyme_src_dir ../.. )
include( ${CMAKE_CURRENT_SOURCE_DIR}/../../../CMakeGlobal/tests/CMakeLists.txt )
