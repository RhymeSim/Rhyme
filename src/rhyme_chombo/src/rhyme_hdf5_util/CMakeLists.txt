cmake_minimum_required( VERSION 3.7 )
enable_language( Fortran )

project( rhyme_hdf5_util )

if ( "${CMAKE_Fortran_COMPILER_ID}" STREQUAL "Intel" )
  set( CMAKE_Fortran_FLAGS "-stand f08" )
elseif( "${CMAKE_Fortran_COMPILER_ID}" STREQUAL "GNU" )
  set( CMAKE_Fortran_FLAGS "-std=f2008" )
endif()

set( deps )
set( internal_deps )

set( srcs
  src/rhyme_hdf5_util.f90
)

set( proj_src_dir ../../.. )
include( ${CMAKE_CURRENT_SOURCE_DIR}/../../../../CMakeGlobal/CMakeLists.txt )

find_package( HDF5 REQUIRED COMPONENTS Fortran )
target_link_libraries( ${PROJECT_NAME} PRIVATE ${HDF5_Fortran_LIBRARIES} )
target_include_directories( ${PROJECT_NAME} PRIVATE ${HDF5_Fortran_INCLUDE_DIRS} )
