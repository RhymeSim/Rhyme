cmake_minimum_required ( VERSION 3.7 )

project ( rhyme_assertion )

enable_language ( Fortran )
add_compile_options ( -Wall -Wextra )
set ( CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -std=f2008" )


set ( LIB_DIR ${CMAKE_BINARY_DIR}/lib CACHE PATH "Lib directory" )
set ( INCLUDE_DIR ${CMAKE_BINARY_DIR}/include CACHE PATH "Include directory" )


set ( CMAKE_Fortran_MODULE_DIRECTORY ${INCLUDE_DIR} )


set ( deps
)

foreach ( dep ${deps} )
  if ( NOT TARGET ${dep} )
    add_subdirectory ( ../${dep} ${CMAKE_BINARY_DIR}/${dep} )
  endif ()
endforeach ( dep )


set ( srcs
  src/rhyme_assertion_add_test_message.f90
  src/rhyme_assertion_to_be_array.f90
  src/rhyme_assertion_to_be.f90
  src/rhyme_assertion_expect.f90
  src/rhyme_assertion_describe.f90
  src/rhyme_assertion.f90
)

add_library ( ${PROJECT_NAME} STATIC ${srcs} )
target_link_libraries ( ${PROJECT_NAME} PRIVATE ${deps} )


add_subdirectory ( tests )
enable_testing ()
