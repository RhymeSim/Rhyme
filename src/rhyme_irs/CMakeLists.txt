cmake_minimum_required( VERSION 3.7 )
enable_language( Fortran )

project( rhyme_irs )

set( deps
  rhyme_hydro_base
  rhyme_ideal_gas
  rhyme_log
)

set( internal_deps
  rhyme_riemann_problem
)

set( srcs
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

set( proj_src_dir .. )
include( ${CMAKE_CURRENT_SOURCE_DIR}/../../CMakeGlobal/CMakeLists.txt )
