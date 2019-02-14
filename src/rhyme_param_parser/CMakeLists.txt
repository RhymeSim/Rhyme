cmake_minimum_required ( VERSION 3.7 )

project ( rhyme_param_parser )
enable_language ( Fortran )
add_compile_options ( -Wall -Wextra -std=f2008 )


set ( LIB_DIR ${CMAKE_BINARY_DIR}/lib CACHE PATH "Lib directory" )
set ( INCLUDE_DIR ${CMAKE_BINARY_DIR}/include CACHE PATH "Include directory" )


set ( CMAKE_Fortran_MODULE_DIRECTORY ${INCLUDE_DIR} )


set ( deps
  rhyme_samr
  rhyme_samr_bc
  rhyme_cfl
  rhyme_ideal_gas
  rhyme_initial_condition
  rhyme_iterative_riemann_solver
  rhyme_slope_limiter
  rhyme_chombo
)

foreach ( dep ${deps} )
  if( NOT TARGET ${dep} )
    add_subdirectory ( ../${dep} ${CMAKE_BINARY_DIR}/${dep} )
  endif ()
endforeach ( dep )


add_library ( ${PROJECT_NAME} STATIC src/rhyme_param_parser.f90 )
target_link_libraries ( ${PROJECT_NAME} PRIVATE ${deps} )

add_subdirectory ( tests )
enable_testing ()
