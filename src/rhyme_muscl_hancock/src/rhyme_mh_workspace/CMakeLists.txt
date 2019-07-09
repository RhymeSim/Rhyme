cmake_minimum_required( VERSION 3.12 )
enable_language( Fortran )

project( rhyme_mh_workspace )

if( "${CMAKE_Fortran_COMPILER_ID}" STREQUAL "Intel" )
  set( CMAKE_Fortran_FLAGS "-stand f08" )
elseif( "${CMAKE_Fortran_COMPILER_ID}" STREQUAL "GNU" )
  set( CMAKE_Fortran_FLAGS "-std=f2008" )
endif()

set( deps
  rhyme_hydro_base
  rhyme_samr
  rhyme_logger
)

set( internal_deps )

set( srcs
  src/rhyme_mh_workspace_check.f90
  src/rhyme_mh_workspace_init.f90
  src/rhyme_mh_workspace.f90
)

set( rhyme_src_dir ../../.. )
include( ${CMAKE_CURRENT_SOURCE_DIR}/../../../../CMakeGlobal/CMakeLists.txt )

# Configs
include( ${CMAKE_CURRENT_SOURCE_DIR}/${rhyme_src_dir}/../CMakeGlobal/configs/CMakeLists.txt )
