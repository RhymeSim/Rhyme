cmake_minimum_required(VERSION 3.12)
enable_language(Fortran)

project(rhyme_report)

if("${CMAKE_Fortran_COMPILER_ID}" STREQUAL "Intel")
  set(CMAKE_Fortran_FLAGS "-stand f08")
elseif("${CMAKE_Fortran_COMPILER_ID}" STREQUAL "GNU")
  set(CMAKE_Fortran_FLAGS "-std=f2008")
endif()

set(deps rhyme_physics rhyme_samr rhyme_logger)

set(internal_deps)

set(srcs
    src/rhyme_report_publish.f90
    src/rhyme_report_plot_histograms.f90
    src/rhyme_report_plot_phase_diagrams.f90
    src/rhyme_report_plot_pseudocolors.f90
    src/rhyme_report_new_histogram.f90
    src/rhyme_report_new_2d_histogram.f90
    src/rhyme_report_new_pseudocolor.f90
    src/rhyme_report_init.f90
    src/rhyme_report.f90)

set(rhyme_src_dir ..)
include(${CMAKE_CURRENT_SOURCE_DIR}/${rhyme_src_dir}/../cmake/helper.cmake)

# Configs
include(${CMAKE_CURRENT_SOURCE_DIR}/${rhyme_src_dir}/../config.cmake)
