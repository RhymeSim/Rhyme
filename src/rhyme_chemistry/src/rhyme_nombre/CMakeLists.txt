cmake_minimum_required (VERSION 3.10)

project (rhyme_nombre)
enable_language (Fortran)
add_compile_options (-Wall -Wextra -std=f2008)

set (CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/include)


file (GLOB_RECURSE srcs src/*.f90)
add_library (rhyme_nombre STATIC ${srcs})

add_subdirectory(tests)

enable_testing ()
