cmake_minimum_required( VERSION 3.7 )
enable_language( Fortran )

project( rhyme_hdf5_util )

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
