cmake_minimum_required ( VERSION 3.10 )

project ( rhyme )
enable_language ( Fortran )
add_compile_options ( -Wall -Wextra -std=f2008 )


set ( deps rhyme_nombre rhyme_samr rhyme_samr_boundary_condition )

foreach ( dep ${deps} )
  if( NOT TARGET ${dep} )
    add_subdirectory ( src/${dep} )
  endif()
endforeach(dep)


set ( CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/include )


file ( GLOB internal_modules RELATIVE ${PROJECT_SOURCE_DIR} src/*_module.f90 )

add_executable ( rhyme src/main.f90 ${internal_modules} )
target_link_libraries ( rhyme ${deps} )


# add_subdirectory (tests)
# enable_testing ()
