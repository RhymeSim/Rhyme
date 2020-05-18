cmake_minimum_required(VERSION 3.12)
enable_language(Fortran)

project(rhyme_plotter)

if("${CMAKE_Fortran_COMPILER_ID}" STREQUAL "Intel")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -stand f08")
elseif("${CMAKE_Fortran_COMPILER_ID}" STREQUAL "GNU")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -std=f2008")
endif()

set(deps)

set(internal_deps rhyme_color)

set(srcs
    src/rhyme_plotter_2d_histogram.f90
    src/rhyme_plotter_histogram.f90
    src/rhyme_plotter_canvas_draw_image.f90
    src/rhyme_plotter_canvas_draw_2d_histogram.f90
    src/rhyme_plotter_canvas_draw_histogram.f90
    src/rhyme_plotter_canvas_plot.f90
    src/rhyme_plotter_canvas_add_colorbar.f90
    src/rhyme_plotter_canvas_add_corner.f90
    src/rhyme_plotter_canvas_add_axis.f90
    src/rhyme_plotter_canvas_clear.f90
    src/rhyme_plotter_canvas_init.f90
    src/rhyme_plotter.f90)

set(rhyme_src_dir ../../..)
include(${CMAKE_CURRENT_SOURCE_DIR}/${rhyme_src_dir}/../cmake/helper.cmake)
