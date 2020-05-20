cmake_minimum_required(VERSION 3.12)
enable_language(Fortran)

project(rhyme_drawing)

if("${CMAKE_Fortran_COMPILER_ID}" STREQUAL "Intel")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -stand f08")
elseif("${CMAKE_Fortran_COMPILER_ID}" STREQUAL "GNU")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -std=f2008")
endif()

set(deps
    rhyme_physics
    rhyme_chemistry
    rhyme_thermo_base
    rhyme_hydro_base
    rhyme_initial_condition
    rhyme_ionisation_equilibrium
    rhyme_samr
    rhyme_logger)

set(internal_deps)

set(srcs
    src/rhyme_drawing_new_perturb.f90
    src/rhyme_drawing_new_shape.f90
    src/rhyme_drawing_apply_perturbations.f90
    src/rhyme_drawing_smoothed_slab_2d.f90
    src/rhyme_drawing_uniform_prism.f90
    src/rhyme_drawing_sphere.f90
    src/rhyme_drawing_uniform_cuboid.f90
    src/rhyme_drawing_uniform_canvas.f90
    src/rhyme_drawing_init.f90
    src/rhyme_drawing.f90)

set(rhyme_src_dir ..)
include(${CMAKE_CURRENT_SOURCE_DIR}/${rhyme_src_dir}/../cmake/helper.cmake)

# Configs
include(${CMAKE_CURRENT_SOURCE_DIR}/${rhyme_src_dir}/../config.cmake)
