cmake_minimum_required(VERSION 3.12)
enable_language(Fortran)

project(rhyme_irs)

if("${CMAKE_Fortran_COMPILER_ID}" STREQUAL "Intel")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -stand f08")
elseif("${CMAKE_Fortran_COMPILER_ID}" STREQUAL "GNU")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -std=f2008")
endif()

set(deps rhyme_physics rhyme_hydro_base rhyme_thermo_base rhyme_logger)

set(internal_deps rhyme_riemann_problem)

set(srcs
    src/rhyme_irs_guess_p_star.f90
    src/rhyme_irs_nonlinear_wave_function.f90
    src/rhyme_irs_rp_side_to_cons.f90
    src/rhyme_irs_w_starR_fan.f90
    src/rhyme_irs_w_starL_fan.f90
    src/rhyme_irs_w_kfan.f90
    src/rhyme_irs_w_starR_sho.f90
    src/rhyme_irs_w_starL_sho.f90
    src/rhyme_irs_w_k.f90
    src/rhyme_irs_sampling.f90
    src/rhyme_irs_iterate.f90
    src/rhyme_irs_solve.f90
    src/rhyme_irs.f90)

set(rhyme_src_dir ..)
include(${CMAKE_CURRENT_SOURCE_DIR}/${rhyme_src_dir}/../cmake/helper.cmake)

# Configs
include(${CMAKE_CURRENT_SOURCE_DIR}/${rhyme_src_dir}/../config.cmake)
