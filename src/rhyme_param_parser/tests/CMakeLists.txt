project( rhyme_param_parser_tests )
set( test_subject rhyme_param_parser )

set( factories )
set( test_deps )

configure_file(
  "../../../parameters.conf.example"
  "parameters.conf.example"
  COPYONLY
)

set( proj_src_dir ../.. )
include( ${CMAKE_CURRENT_SOURCE_DIR}/../../../CMakeGlobal/tests/CMakeLists.txt )
