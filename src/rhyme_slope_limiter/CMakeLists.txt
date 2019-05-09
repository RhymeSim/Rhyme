cmake_minimum_required( VERSION 3.7 )
enable_language( Fortran )

project( rhyme_slope_limiter )

set( deps
  rhyme_cfl
  rhyme_hydro_base
  rhyme_log
)

set( internal_deps )

set( srcs
  src/rhyme_slope_limiter_superbee.f90
  src/rhyme_slope_limiter_van_albada.f90
  src/rhyme_slope_limiter_minmod.f90
  src/rhyme_slope_limiter_van_leer.f90
  src/rhyme_slope_limiter_xi_r.f90
  src/rhyme_slope_limiter_xi_l.f90
  src/rhyme_slope_limiter_run.f90
  src/rhyme_slope_limiter.f90
)

set( proj_src_dir .. )
include( ${CMAKE_CURRENT_SOURCE_DIR}/../../CMakeGlobal/CMakeLists.txt )
