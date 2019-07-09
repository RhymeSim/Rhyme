cmake_minimum_required( VERSION 3.12 )

project( rhyme )
enable_language( Fortran )

# Check config file (CMakeGlobal/config/CMakeLists.txt) before compiling

if ( "${CMAKE_Fortran_COMPILER_ID}" STREQUAL "Intel" )
  add_compile_options( -warn all -fpp -O3 )
  set( CMAKE_Fortran_FLAGS "-stand f08" )
elseif( "${CMAKE_Fortran_COMPILER_ID}" STREQUAL "GNU" )
  add_compile_options( -Wall -Wextra -cpp -O3 ) # -fopt-info )
  set( CMAKE_Fortran_FLAGS "-std=f2008" )
endif()


set( rhyme_src_dir src )
include( ${CMAKE_CURRENT_SOURCE_DIR}/CMakeGlobal/configs/CMakeLists.txt )


set( LIB_DIR ${CMAKE_BINARY_DIR}/lib CACHE PATH "Lib directory" )
set( INCLUDE_DIR ${CMAKE_BINARY_DIR}/include CACHE PATH "Include directory" )


set( CMAKE_Fortran_MODULE_DIRECTORY ${INCLUDE_DIR} )


# find_package( OpenMP )
#
# if( OPENMP_FOUND )
#   set( CMAKE_Fortran_FLAGS
#     "${CMAKE_Fortran_FLAGS} ${OpenMP_Fortran_FLAGS}" )
#   set( CMAKE_C_FLAGS "${CMAKE_C_FLAGS} ${OpenMP_C_FLAGS}" )
#   set( CMAKE_EXE_LINKER_FLAGS
#     "${CMAKE_EXE_LINKER_FLAGS} ${OpenMP_EXE_LINKER_FLAGS}" )
# endif()


set( deps
  rhyme_logger
  rhyme_nombre
  rhyme_physics
  rhyme_hydro_base
  rhyme_thermo_base
  rhyme_chemistry
  rhyme_samr
  rhyme_samr_bc
  rhyme_cfl
  rhyme_drawing
  rhyme_slope_limiter
  rhyme_irs
  rhyme_muscl_hancock
  rhyme_param_parser
  rhyme_muscl_hancock
  rhyme_chombo
  rhyme_initial_condition
  radamesh_spectrum
)

foreach( dep ${deps} )
  if( NOT TARGET ${dep} )
    add_subdirectory( src/${dep} ${CMAKE_BINARY_DIR}/${dep} )
  endif()
endforeach( dep )


file( GLOB internal_modules RELATIVE ${PROJECT_SOURCE_DIR} src/*_module.f90 )


set( srcs
  src/main.f90
)

add_executable( rhyme_${NUMBER_OF_DIM}d ${srcs} ${internal_modules} )
target_link_libraries( rhyme_${NUMBER_OF_DIM}d ${deps} )

enable_testing()
