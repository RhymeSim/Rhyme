cmake_minimum_required ( VERSION 3.5 )

project ( rhyme_param_parser )
enable_language ( Fortran )
add_compile_options ( -Wall -Wextra -std=f2008 )


set ( deps rhyme_samr rhyme_samr_boundary_condition )

foreach ( dep ${deps} )
  if( NOT TARGET ${dep} )
    add_subdirectory ( src/${dep} )
  endif()
endforeach(dep)


set ( CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/include )


add_library ( ${PROJECT_NAME} STATIC src/rhyme_param_parser.f90 )
target_link_libraries ( ${PROJECT_NAME} ${deps} )

add_subdirectory ( tests )
enable_testing ()
