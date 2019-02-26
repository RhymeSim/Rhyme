cmake_minimum_required ( VERSION 3.7 )

project ( rhyme )
enable_language ( Fortran )
add_compile_options ( -Wall -Wextra -std=f2008 )


set ( LIB_DIR ${CMAKE_BINARY_DIR}/lib CACHE PATH "Lib directory" )
set ( INCLUDE_DIR ${CMAKE_BINARY_DIR}/include CACHE PATH "Include directory" )


set ( CMAKE_Fortran_MODULE_DIRECTORY ${INCLUDE_DIR} )


set ( deps
  rhyme_log
  rhyme_nombre
  rhyme_hydro_base
  rhyme_samr
  rhyme_samr_bc
  rhyme_cfl
  rhyme_ideal_gas
  rhyme_drawing
  rhyme_slope_limiter
  rhyme_iterative_riemann_solver
  rhyme_muscl_hancock
  rhyme_param_parser
  rhyme_muscl_hancock
  rhyme_chombo
  rhyme_initial_condition
  radamesh_spectrum
)

foreach ( dep ${deps} )
  if ( NOT TARGET ${dep} )
    add_subdirectory ( src/${dep} ${CMAKE_BINARY_DIR}/${dep} )
  endif ()
endforeach ( dep )


file ( GLOB internal_modules RELATIVE ${PROJECT_SOURCE_DIR} src/*_module.f90 )


add_executable ( rhyme src/main.f90 ${internal_modules} )
target_link_libraries ( rhyme ${deps} )


add_subdirectory ( tests )
enable_testing ()
