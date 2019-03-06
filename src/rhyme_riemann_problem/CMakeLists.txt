cmake_minimum_required ( VERSION 3.5 )

project ( rhyme_riemann_problem )

enable_language ( Fortran )
add_compile_options ( -Wall -Wextra )
set ( CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -std=f2008" )


set ( LIB_DIR ${CMAKE_BINARY_DIR}/lib CACHE PATH "Lib directory" )
set ( INCLUDE_DIR ${CMAKE_BINARY_DIR}/include CACHE PATH "Include directory" )


set ( CMAKE_Fortran_MODULE_DIRECTORY ${INCLUDE_DIR} )


set ( srcs
  src/rhyme_riemann_problem.f90
)


add_library ( ${PROJECT_NAME} STATIC ${srcs} )


add_subdirectory ( tests )
enable_testing ()
