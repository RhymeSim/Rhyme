project ( rhyme_irs_tests )

enable_language ( C Fortran )
add_compile_options ( -Wall -Wextra )
set ( CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -std=f2008" )
set ( CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -std=c11" )


file ( GLOB_RECURSE test_files RELATIVE ${PROJECT_SOURCE_DIR} *_test.f90 )
file ( GLOB_RECURSE factory_files RELATIVE ${PROJECT_SOURCE_DIR} *_factory.f90 )

configure_file ( "sod_shock_tube_t_0_2_analytical.txt" "sod_shock_tube_t_0_2_analytical.txt" COPYONLY )


foreach ( test_file ${test_files} )
  get_filename_component ( test_lib ${test_file} NAME_WE )
  list ( APPEND test_libs ${test_lib}_ )
endforeach(test_file)


create_test_sourcelist ( _ main.c ${test_libs} )


set ( INCLUDE_DIR ${CMAKE_BINARY_DIR}/include CACHE PATH "Include directory" )
set ( CMAKE_Fortran_MODULE_DIRECTORY ${INCLUDE_DIR} )


set ( factories
  rhyme_hydro_base
)

foreach ( factory ${factories} )
  list ( APPEND factory_files ../../${factory}/tests/${factory}_factory.f90 )

  if ( NOT TARGET ${factory} )
    add_subdirectory ( ../../${factory} ${CMAKE_BINARY_DIR}/${factory} )
  endif ()
endforeach ( factory )

add_library ( ${PROJECT_NAME}_fortran STATIC ${test_files} ${factory_files} )
add_dependencies( ${PROJECT_NAME}_fortran ${factories} rhyme_irs )

add_executable ( ${PROJECT_NAME} main.c )
target_link_libraries ( ${PROJECT_NAME} ${PROJECT_NAME}_fortran rhyme_irs )


set ( idx 0 )
list ( LENGTH test_files len )

while ( ${len} GREATER ${idx} )
  list ( GET test_files ${idx} test_file )
  list ( GET test_libs ${idx} test_lib )

  add_test ( NAME ${test_file} COMMAND $<TARGET_FILE:${PROJECT_NAME}> ${test_lib} )

  math ( EXPR idx "${idx} + 1" )
endwhile ()


enable_testing ()
