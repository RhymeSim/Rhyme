cmake_minimum_required ( VERSION 3.7 )

project ( rhyme_ideal_gas )
enable_language ( Fortran )
add_compile_options ( -Wall -Wextra -std=f2008 )


set ( LIB_DIR ${CMAKE_BINARY_DIR}/lib CACHE PATH "Lib directory" )
set ( INCLUDE_DIR ${CMAKE_BINARY_DIR}/include CACHE PATH "Include directory" )


set ( CMAKE_Fortran_MODULE_DIRECTORY ${INCLUDE_DIR} )


set ( deps
  rhyme_thermo_base
  rhyme_hydro_base
  rhyme_nombre
  rhyme_chemistry
)

foreach ( dep ${deps} )
  if ( NOT TARGET ${dep} )
    add_subdirectory ( ../${dep} ${CMAKE_BINARY_DIR}/${dep} )
  endif ()
endforeach ( dep )


find_package ( HDF5 REQUIRED )


add_library ( ${PROJECT_NAME} STATIC src/rhyme_ideal_gas.f90 )
target_link_libraries ( ${PROJECT_NAME} PRIVATE ${deps} hdf5 hdf5_fortran )


add_subdirectory ( tests )
enable_testing ()
