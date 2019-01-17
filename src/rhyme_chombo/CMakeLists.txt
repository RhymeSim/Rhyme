cmake_minimum_required ( VERSION 3.7 )

project ( rhyme_chombo )
enable_language ( C Fortran )
add_compile_options ( -Wall -Wextra -std=f2008 )


set ( LIB_DIR ${CMAKE_BINARY_DIR}/lib CACHE PATH "Lib directory" )
set ( INCLUDE_DIR ${CMAKE_BINARY_DIR}/include CACHE PATH "Include directory" )


set ( CMAKE_Fortran_MODULE_DIRECTORY ${INCLUDE_DIR} )



set ( deps
  rhyme_samr
)

foreach ( dep ${deps} )
  if ( NOT TARGET ${dep} )
    add_subdirectory ( ../${dep} ${CMAKE_BINARY_DIR}/${dep} )
  endif ()
endforeach ( dep )

add_subdirectory ( src/rhyme_hdf5_util )


add_library ( ${PROJECT_NAME} STATIC src/rhyme_chombo.f90 )
target_link_libraries( ${PROJECT_NAME} PRIVATE ${deps} rhyme_hdf5_util )


add_subdirectory ( tests )
enable_testing ()
