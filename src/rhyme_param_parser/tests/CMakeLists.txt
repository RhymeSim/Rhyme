project( rhyme_param_parser_tests )
set( test_subject rhyme_param_parser )

set( factories )

set( test_deps
  rhyme_assertion
)

configure_file(
  "../../../parameters.conf.example"
  "parameters.conf.example"
  COPYONLY
)

configure_file(
  "./assets/occurences.txt"
  "occurences.txt"
  COPYONLY
)

set( rhyme_src_dir ../.. )
include( ${CMAKE_CURRENT_SOURCE_DIR}/../../../CMakeGlobal/tests/CMakeLists.txt )
