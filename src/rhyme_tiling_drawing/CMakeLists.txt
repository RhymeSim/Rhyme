cmake_minimum_required(VERSION 3.12)
enable_language(Fortran)

project(rhyme_tiling_drawing)

if("${CMAKE_Fortran_COMPILER_ID}" STREQUAL "Intel")
  set(CMAKE_Fortran_FLAGS "-stand f08")
elseif("${CMAKE_Fortran_COMPILER_ID}" STREQUAL "GNU")
  set(CMAKE_Fortran_FLAGS "-std=f2008")
endif()

set(deps rhyme_nombre rhyme_tiling rhyme_logger)

set(internal_deps)

set(srcs
    src/rhyme_tiling_drawing_add_shape.f90 src/rhyme_tiling_drawing_sphere.f90
    src/rhyme_tiling_drawing_init.f90 src/rhyme_tiling_drawing.f90)

set(rhyme_src_dir ..)
include(${CMAKE_CURRENT_SOURCE_DIR}/${rhyme_src_dir}/../cmake/helper.cmake)

# Configs
include(${CMAKE_CURRENT_SOURCE_DIR}/${rhyme_src_dir}/../config.cmake)
