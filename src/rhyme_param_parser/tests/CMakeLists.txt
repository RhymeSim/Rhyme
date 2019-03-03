project( rhyme_param_parser_tests )

enable_language ( C Fortran )
enable_testing ()


file ( GLOB_RECURSE test_files RELATIVE ${PROJECT_SOURCE_DIR} *_test.f90 )

configure_file (
  "../../../parameters.conf.example"
  "parameters.conf.example"
  COPYONLY
)


set ( test_libs )

foreach ( test_file ${test_files} )
  get_filename_component ( test_lib ${test_file} NAME_WE )
  list ( APPEND test_libs ${test_lib}_ )
endforeach(test_file)


create_test_sourcelist ( _ main.c ${test_libs} )


add_library ( ${PROJECT_NAME}_fortran ${test_files} )
add_executable ( ${PROJECT_NAME} main.c )
target_link_libraries ( ${PROJECT_NAME} ${PROJECT_NAME}_fortran rhyme_param_parser)


set ( idx 0 )
list ( LENGTH test_files len )

while ( ${len} GREATER ${idx} )
  list ( GET test_files ${idx} test_file )
  list ( GET test_libs ${idx} test_lib )

  add_test ( NAME ${test_file} COMMAND $<TARGET_FILE:${PROJECT_NAME}> ${test_lib})

  math ( EXPR idx "${idx} + 1" )
endwhile ()
