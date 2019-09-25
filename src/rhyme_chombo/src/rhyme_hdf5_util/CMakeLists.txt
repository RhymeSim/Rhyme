cmake_minimum_required( VERSION 3.12 )
enable_language( Fortran )

project( rhyme_hdf5_util )

if ( "${CMAKE_Fortran_COMPILER_ID}" STREQUAL "Intel" )
  set( CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -stand f08" )
elseif( "${CMAKE_Fortran_COMPILER_ID}" STREQUAL "GNU" )
  set( CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -std=f2008" )
endif()

set( deps )
set( internal_deps )

set( srcs
  src/rhyme_hdf5_util_close.f90
  src/rhyme_hdf5_util_read_2d_dataset.f90
  src/rhyme_hdf5_util_read_1d_dataset.f90
  src/rhyme_hdf5_util_write_2d_dataset.f90
  src/rhyme_hdf5_util_write_1d_dataset.f90
  src/rhyme_hdf5_util_read_group_comp_1d_array_attr.f90
  src/rhyme_hdf5_util_read_group_1d_array_attr.f90
  src/rhyme_hdf5_util_read_group_attr.f90
  src/rhyme_hdf5_util_get_table_size.f90
  src/rhyme_hdf5_util_read_table.f90
  src/rhyme_hdf5_util_write_table.f90
  src/rhyme_hdf5_util_write_group_comp_1d_array_attr.f90
  src/rhyme_hdf5_util_write_group_1d_array_attr.f90
  src/rhyme_hdf5_util_write_group_attr.f90
  src/rhyme_hdf5_util_create_group.f90
  src/rhyme_hdf5_util_create.f90
  src/rhyme_hdf5_util_open.f90
  src/rhyme_hdf5_util.f90
)

set( rhyme_src_dir ../../.. )
include( ${CMAKE_CURRENT_SOURCE_DIR}/../../../../CMakeGlobal/CMakeLists.txt )

find_package( HDF5 REQUIRED COMPONENTS Fortran )
target_link_libraries( ${PROJECT_NAME} PRIVATE ${HDF5_Fortran_LIBRARIES} )
target_include_directories( ${PROJECT_NAME} PRIVATE ${HDF5_Fortran_INCLUDE_DIRS} )
