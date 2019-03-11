cmake_minimum_required ( VERSION 3.7 )

project ( rhyme_muscl_hancock )

enable_language ( Fortran )
add_compile_options ( -Wall -Wextra )
set ( CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -std=f2008" )


set ( LIB_DIR ${CMAKE_BINARY_DIR}/lib CACHE PATH "Lib directory" )
set ( INCLUDE_DIR ${CMAKE_BINARY_DIR}/include CACHE PATH "Include directory" )


set ( CMAKE_Fortran_MODULE_DIRECTORY ${INCLUDE_DIR} )


# find_package ( OpenMP )
#
# if ( OPENMP_FOUND )
#   set ( CMAKE_Fortran_FLAGS
#     "${CMAKE_Fortran_FLAGS} ${OpenMP_Fortran_FLAGS}" )
#   set ( CMAKE_C_FLAGS "${CMAKE_C_FLAGS} ${OpenMP_C_FLAGS}" )
#   set ( CMAKE_EXE_LINKER_FLAGS
#     "${CMAKE_EXE_LINKER_FLAGS} ${OpenMP_EXE_LINKER_FLAGS}" )
# endif ( OPENMP_FOUND )


set ( deps
  rhyme_cfl
  rhyme_ideal_gas
  rhyme_slope_limiter
  rhyme_iterative_riemann_solver
  rhyme_log
)

foreach ( dep ${deps} )
  if ( NOT TARGET ${dep} )
    add_subdirectory ( ../${dep} ${CMAKE_BINARY_DIR}/${dep} )
  endif ()
endforeach ( dep )

add_subdirectory ( src/rhyme_mh_workspace )


set ( srcs
  src/rhyme_muscl_hancock.f90
)

add_library ( ${PROJECT_NAME} STATIC ${srcs} )
target_link_libraries ( ${PROJECT_NAME} PRIVATE ${deps} rhyme_mh_workspace )


add_subdirectory ( tests )
enable_testing ()
