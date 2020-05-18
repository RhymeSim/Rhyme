cmake_minimum_required(VERSION 3.12)
enable_language(Fortran)

project(rhyme_logger)

if("${CMAKE_Fortran_COMPILER_ID}" STREQUAL "Intel")
  set(CMAKE_Fortran_FLAGS
      "${CMAKE_Fortran_FLAGS} -stand f08 -extend-source 512")
elseif("${CMAKE_Fortran_COMPILER_ID}" STREQUAL "GNU")
  set(CMAKE_Fortran_FLAGS
      "${CMAKE_Fortran_FLAGS} -std=f2008 -ffree-line-length-512")
endif()

set(deps rhyme_string)

set(internal_deps rhyme_plotter)

set(srcs
    src/rhyme_logger.f90
    src/rhyme_logger_log.f90
    src/rhyme_logger_warn.f90
    src/rhyme_logger_err.f90
    src/rhyme_logger_io.f90
    src/rhyme_logger_logo.f90
    src/rhyme_logger_time.f90
    src/rhyme_logger_plot_image.f90
    src/rhyme_logger_section.f90)

set(rhyme_src_dir ..)
include(${CMAKE_CURRENT_SOURCE_DIR}/${rhyme_src_dir}/../cmake/helper.cmake)
