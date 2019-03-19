cmake_minimum_required ( VERSION 3.7 )

project ( rhyme_irs )

enable_language ( Fortran )
add_compile_options ( -Wall -Wextra )
set ( CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -std=f2008" )


set ( LIB_DIR ${CMAKE_BINARY_DIR}/lib CACHE PATH "Lib directory" )
set ( INCLUDE_DIR ${CMAKE_BINARY_DIR}/include CACHE PATH "Include directory" )


set ( CMAKE_Fortran_MODULE_DIRECTORY ${INCLUDE_DIR} )


set ( deps
  rhyme_hydro_base
  rhyme_ideal_gas
  rhyme_log
)

foreach ( dep ${deps} )
  if ( NOT TARGET ${dep} )
    add_subdirectory ( ../${dep} ${CMAKE_BINARY_DIR}/${dep} )
  endif ()
endforeach ( dep )


set ( internal_deps
  rhyme_riemann_problem
)

foreach ( i_dep ${internal_deps} )
  if ( NOT TARGET ${i_dep} )
    add_subdirectory ( src/${i_dep} )
  endif ()
endforeach ( i_dep )


set ( srcs
  src/rhyme_irs.f90
  src/rhyme_irs_solve.f90
  src/rhyme_irs_iterate.f90
  src/rhyme_irs_sampling.f90
  src/rhyme_irs_w_k.f90
  src/rhyme_irs_w_starL_sho.f90
  src/rhyme_irs_w_starR_sho.f90
  src/rhyme_irs_w_kfan.f90
  src/rhyme_irs_w_starL_fan.f90
  src/rhyme_irs_w_starR_fan.f90
  src/rhyme_irs_rp_side_to_cons.f90
  src/rhyme_irs_nonlinear_wave_function.f90
  src/rhyme_irs_guess_p_star.f90
)


add_library ( ${PROJECT_NAME} STATIC ${srcs} )
target_link_libraries ( ${PROJECT_NAME} ${deps} ${internal_deps} )


add_subdirectory (tests)
enable_testing ()
