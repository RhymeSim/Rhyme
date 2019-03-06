cmake_minimum_required ( VERSION 3.7 )

project ( rhyme_chemistry )

enable_language ( Fortran )
add_compile_options ( -Wall -Wextra )
set ( CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -std=f2008" )


set ( LIB_DIR ${CMAKE_BINARY_DIR}/lib CACHE PATH "Lib directory" )
set ( INCLUDE_DIR ${CMAKE_BINARY_DIR}/include CACHE PATH "Include directory" )


set ( CMAKE_Fortran_MODULE_DIRECTORY ${INCLUDE_DIR} )


set ( deps
  rhyme_nombre
)

foreach ( dep ${deps} )
  if ( NOT TARGET ${dep} )
    add_subdirectory ( ../../../${dep} ${CMAKE_BINARY_DIR}/${deps} )
  endif ()
endforeach ( dep )


set ( srcs
  src/rhyme_chemistry.f90
)


add_library ( ${PROJECT_NAME} STATIC ${srcs} )
target_link_libraries ( ${PROJECT_NAME} ${deps} )


add_subdirectory ( tests )
enable_testing ()
