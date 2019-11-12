project( rhyme_param_parser_tests )
set( test_subject rhyme_param_parser )

set( factories
  rhyme_logger
)

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
include( ${CMAKE_CURRENT_SOURCE_DIR}/${rhyme_src_dir}/../cmake/test_helper.cmake )

# Configs
include( ${CMAKE_CURRENT_SOURCE_DIR}/${rhyme_src_dir}/../config.cmake )
