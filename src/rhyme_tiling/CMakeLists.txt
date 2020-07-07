cmake_minimum_required(VERSION 3.12)
enable_language(Fortran)

project(rhyme_tiling)

if("${CMAKE_Fortran_COMPILER_ID}" STREQUAL "Intel")
  set(CMAKE_Fortran_FLAGS "-stand f08")
elseif("${CMAKE_Fortran_COMPILER_ID}" STREQUAL "GNU")
  set(CMAKE_Fortran_FLAGS "-std=f2008")
endif()

set(deps rhyme_logger)

set(internal_deps)

set(srcs
    src/rhyme_tiling_find_max.f90
    src/rhyme_tiling_add_tile.f90
    src/rhyme_tiling_find_tile.f90
    src/rhyme_tiling_init.f90
    src/rhyme_tiling_to_coordinate.f90
    src/rhyme_tiling_level_tile_ids.f90
    src/rhyme_tiling_total_ntiles.f90
    src/rhyme_tiling.f90)

set(rhyme_src_dir ..)
include(${CMAKE_CURRENT_SOURCE_DIR}/${rhyme_src_dir}/../cmake/helper.cmake)

# Configs
include(${CMAKE_CURRENT_SOURCE_DIR}/${rhyme_src_dir}/../config.cmake)
