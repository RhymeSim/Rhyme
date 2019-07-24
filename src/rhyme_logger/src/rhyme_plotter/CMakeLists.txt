cmake_minimum_required( VERSION 3.12 )
enable_language( Fortran )

project( rhyme_plotter )

if ( "${CMAKE_Fortran_COMPILER_ID}" STREQUAL "Intel" )
  set( CMAKE_Fortran_FLAGS "-stand f08 -extend-source 512" )
elseif( "${CMAKE_Fortran_COMPILER_ID}" STREQUAL "GNU" )
  set( CMAKE_Fortran_FLAGS "-std=f2008 -ffree-line-length-512" )
endif()

set( deps )

set( internal_deps )

set( srcs
  src/rhyme_plotter_histogram_draw_on.f90
  src/rhyme_plotter_histogram.f90
  src/rhyme_plotter_canvas_reset.f90
  src/rhyme_plotter_canvas_plot.f90
  src/rhyme_plotter_canvas_add_axis.f90
  src/rhyme_plotter_canvas_init.f90
  src/rhyme_plotter.f90
)

set( rhyme_src_dir ../../.. )
include( ${CMAKE_CURRENT_SOURCE_DIR}/../../../../CMakeGlobal/CMakeLists.txt )
