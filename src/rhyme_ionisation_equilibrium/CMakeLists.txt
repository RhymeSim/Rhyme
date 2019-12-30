cmake_minimum_required(VERSION 3.12)
enable_language(Fortran)

project(rhyme_ionisation_equilibrium)

if("${CMAKE_Fortran_COMPILER_ID}" STREQUAL "Intel")
  set(CMAKE_Fortran_FLAGS "-stand f08")
elseif("${CMAKE_Fortran_COMPILER_ID}" STREQUAL "GNU")
  set(CMAKE_Fortran_FLAGS "-std=f2008")
endif()

set(deps
  rhyme_logger
)

set(internal_deps)

set(srcs
  src/rhyme_ionisation_equilibrium_equality.f90
  src/rhyme_ionisation_equilibrium_init.f90
  src/rhyme_ionisation_equilibrium.f90
)

set(rhyme_src_dir ..)
include(${CMAKE_CURRENT_SOURCE_DIR}/../../CMakeGlobal/CMakeLists.txt)

# Configs
include(${CMAKE_CURRENT_SOURCE_DIR}/${rhyme_src_dir}/../CMakeGlobal/configs/CMakeLists.txt)
