cmake_minimum_required ( VERSION 3.7 )

project ( rhyme_hdf5_util )
enable_language ( C Fortran )
add_compile_options ( -Wall -Wextra -std=f2008 )


set ( LIB_DIR ${CMAKE_BINARY_DIR}/lib CACHE PATH "Lib directory" )
set ( INCLUDE_DIR ${CMAKE_BINARY_DIR}/include CACHE PATH "Include directory" )


set ( CMAKE_Fortran_MODULE_DIRECTORY ${INCLUDE_DIR} )


add_library ( ${PROJECT_NAME} STATIC src/rhyme_hdf5_util.f90 )


find_package ( HDF5 REQUIRED COMPONENTS Fortran )
target_link_libraries( ${PROJECT_NAME} PRIVATE ${deps} ${HDF5_Fortran_LIBRARIES} )
target_include_directories( ${PROJECT_NAME} PRIVATE ${HDF5_Fortran_INCLUDE_DIRS} )


add_subdirectory ( tests )
enable_testing ()
