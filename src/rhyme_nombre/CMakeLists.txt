cmake_minimum_required ( VERSION 3.7 )

project ( rhyme_nombre )
enable_language ( Fortran )
add_compile_options ( -Wall -Wextra -std=f2008 )


set ( LIB_DIR "${PROJECT_SOURCE_DIR}/lib" CACHE PATH "lib directory" )
set ( INCLUDE_DIR "${PROJECT_SOURCE_DIR}/include" CACHE PATH "include directory" )


set ( CMAKE_Fortran_MODULE_DIRECTORY ${INCLUDE_DIR} )


set ( srcs
  src/rhyme_dimension.f90
  src/rhyme_nombre.f90
  src/rhyme_prefix.f90
  src/rhyme_unit.f90
  src/rhyme_units.f90
)

add_library ( rhyme_nombre STATIC ${srcs} )


add_subdirectory ( tests )
enable_testing ()
