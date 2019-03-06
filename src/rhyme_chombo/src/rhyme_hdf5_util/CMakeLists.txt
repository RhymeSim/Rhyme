cmake_minimum_required ( VERSION 3.7 )

project ( rhyme_hdf5_util )

enable_language ( Fortran )
add_compile_options ( -Wall -Wextra )
set ( CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -std=f2008" )


set ( LIB_DIR ${CMAKE_BINARY_DIR}/lib CACHE PATH "Lib directory" )
set ( INCLUDE_DIR ${CMAKE_BINARY_DIR}/include CACHE PATH "Include directory" )


set ( CMAKE_Fortran_MODULE_DIRECTORY ${INCLUDE_DIR} )


set ( srcs
  src/rhyme_hdf5_util.f90
)

add_library ( ${PROJECT_NAME} STATIC ${srcs} )


find_package ( HDF5 REQUIRED COMPONENTS Fortran )
target_link_libraries( ${PROJECT_NAME} PRIVATE ${HDF5_Fortran_LIBRARIES} )
target_include_directories( ${PROJECT_NAME} PRIVATE ${HDF5_Fortran_INCLUDE_DIRS} )


add_subdirectory ( tests )
enable_testing ()
