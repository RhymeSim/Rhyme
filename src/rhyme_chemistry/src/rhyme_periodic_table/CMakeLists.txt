cmake_minimum_required(VERSION 3.12)
enable_language(Fortran)

project(rhyme_periodic_table)

if("${CMAKE_Fortran_COMPILER_ID}" STREQUAL "Intel")
  set(CMAKE_Fortran_FLAGS "-stand f08")
elseif("${CMAKE_Fortran_COMPILER_ID}" STREQUAL "GNU")
  set(CMAKE_Fortran_FLAGS "-std=f2008")
endif()

set(deps rhyme_logger)

set(internal_deps)

set(srcs
    src/rhyme_periodic_table_init.f90
    src/rhyme_periodic_table_get_element_by_name.f90
    src/rhyme_periodic_table.f90)

set(rhyme_src_dir ../../..)
include(${CMAKE_CURRENT_SOURCE_DIR}/${rhyme_src_dir}/../cmake/helper.cmake)

# Configs
include(${CMAKE_CURRENT_SOURCE_DIR}/${rhyme_src_dir}/../config.cmake)
