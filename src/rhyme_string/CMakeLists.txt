cmake_minimum_required(VERSION 3.12)
enable_language(Fortran)

project(rhyme_string)

if("${CMAKE_Fortran_COMPILER_ID}" STREQUAL "Intel")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -stand f08")
elseif("${CMAKE_Fortran_COMPILER_ID}" STREQUAL "GNU")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -std=f2008")
endif()

set(deps)
set(internal_deps)

set(srcs src/rhyme_string_get_filename.f90 src/rhyme_string_is_nan.f90
         src/rhyme_string_to_string.f90 src/rhyme_string.f90)

set(rhyme_src_dir ..)
include(${CMAKE_CURRENT_SOURCE_DIR}/${rhyme_src_dir}/../cmake/helper.cmake)
