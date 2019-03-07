cmake_minimum_required ( VERSION 3.7 )

project ( rhyme_ideal_gas )

enable_language ( Fortran )
add_compile_options ( -Wall -Wextra )
set ( CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -std=f2008" )


set ( LIB_DIR ${CMAKE_BINARY_DIR}/lib CACHE PATH "Lib directory" )
set ( INCLUDE_DIR ${CMAKE_BINARY_DIR}/include CACHE PATH "Include directory" )


set ( CMAKE_Fortran_MODULE_DIRECTORY ${INCLUDE_DIR} )


set ( deps
  rhyme_hydro_base
  rhyme_log
)

foreach ( dep ${deps} )
  if ( NOT TARGET ${dep} )
    add_subdirectory ( ../${dep} ${CMAKE_BINARY_DIR}/${dep} )
  endif ()
endforeach ( dep )


set ( internal_deps
  rhyme_chemistry
  rhyme_thermo_base
)

foreach ( i_dep ${internal_deps} )
  if ( NOT TARGET ${i_dep} )
    add_subdirectory ( src/${i_dep} )
  endif ()
endforeach ( i_dep )


set ( srcs
  src/rhyme_ideal_gas.f90
)

add_library ( ${PROJECT_NAME} STATIC ${srcs} )
target_link_libraries ( ${PROJECT_NAME} PRIVATE ${deps} ${internal_deps} )


add_subdirectory ( tests )
enable_testing ()
