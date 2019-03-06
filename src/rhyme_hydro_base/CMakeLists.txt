cmake_minimum_required ( VERSION 3.7 )

project ( rhyme_hydro_base )

enable_language ( Fortran )
add_compile_options ( -Wall -Wextra )
set ( CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -std=f2008" )


set ( LIB_DIR ${CMAKE_BINARY_DIR}/lib CACHE PATH "Lib directory" )
set ( INCLUDE_DIR ${CMAKE_BINARY_DIR}/include CACHE PATH "Include directory" )


set ( CMAKE_Fortran_MODULE_DIRECTORY ${INCLUDE_DIR} )


add_library ( rhyme_hydro_base STATIC src/rhyme_hydro_base.f90 )


add_subdirectory ( tests )
enable_testing ()
