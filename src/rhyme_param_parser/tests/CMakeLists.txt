project( rhyme_param_parser_tests )
set( test_subject rhyme_param_parser )

set( factories )

set( test_deps
  rhyme_assertion
)

configure_file( "../../../parameters_1d.conf.example" "parameters_1d.conf.example" COPYONLY )
configure_file( "../../../parameters_2d.conf.example" "parameters_2d.conf.example" COPYONLY )
configure_file( "../../../parameters_3d.conf.example" "parameters_3d.conf.example" COPYONLY )

configure_file(
  "./assets/occurences.txt"
  "occurences.txt"
  COPYONLY
)

set( rhyme_src_dir ../.. )
include( ${CMAKE_CURRENT_SOURCE_DIR}/../../../CMakeGlobal/tests/CMakeLists.txt )

# Configs
include( ${CMAKE_CURRENT_SOURCE_DIR}/${rhyme_src_dir}/../CMakeGlobal/configs/CMakeLists.txt )
