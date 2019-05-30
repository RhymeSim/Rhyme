cmake_minimum_required( VERSION 3.12 )

project( rhyme )

set( HYDRO_SOLVER ON )
set( RT_SOLVER ON )

set( NUMBER_OF_DIM 3 )
set( NUMBER_OF_SPECIES 3 )


enable_language( Fortran )


if ( "${CMAKE_Fortran_COMPILER_ID}" STREQUAL "Intel" )
  add_compile_options( -warn all -fpp -O3 )
  set( CMAKE_Fortran_FLAGS "-stand f08" )
elseif( "${CMAKE_Fortran_COMPILER_ID}" STREQUAL "GNU" )
  add_compile_options( -Wall -Wextra -cpp -O3 ) # -fopt-info )
  set( CMAKE_Fortran_FLAGS "-std=f2008" )
endif()


# Calculating Number of Components (NCMP)
if( ${HYDRO_SOLVER} )
  # Adding momenta and total energy
  math( EXPR NCMP "1+${NUMBER_OF_DIM}+1" ) # rho, momenta, energy
endif()

if( ${RT_SOLVER} )
  # Adding temperature and ionization fractions
  math( EXPR NCMP "${NCMP}+1+${NUMBER_OF_SPECIES}" ) # temperature, neutral fractions
endif()

add_compile_definitions( NDIM=${NUMBER_OF_DIM} )
add_compile_definitions( NSPE=${NUMBER_OF_SPECIES} )
add_compile_definitions( NCMP=${NCMP} )


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
  rhyme_log
  rhyme_nombre
  rhyme_units
  rhyme_hydro_base
  rhyme_samr
  rhyme_samr_bc
  rhyme_cfl
  rhyme_ideal_gas
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

add_executable( rhyme ${srcs} ${internal_modules} )
target_link_libraries( rhyme ${deps} )

enable_testing()
