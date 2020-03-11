cmake_minimum_required(VERSION 3.12)
enable_language(Fortran)

project(rhyme_assertion)

if("${CMAKE_Fortran_COMPILER_ID}" STREQUAL "Intel")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -stand f08")
elseif("${CMAKE_Fortran_COMPILER_ID}" STREQUAL "GNU")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -std=f2008")
endif()

set(deps rhyme_logger rhyme_string)

set(internal_deps)

set(srcs
    src/rhyme_assertion_failed.f90
    src/rhyme_assertion_passed.f90
    src/rhyme_assertion_within.f90
    src/rhyme_assertion_add_test_message.f90
    src/rhyme_assertion_not_to_be_array_scalar.f90
    src/rhyme_assertion_not_to_be_array.f90
    src/rhyme_assertion_not_to_be_nan_array.f90
    src/rhyme_assertion_not_to_be_nan.f90
    src/rhyme_assertion_not_to_be.f90
    src/rhyme_assertion_to_be_array_3d_scalar.f90
    src/rhyme_assertion_to_be_array_3d.f90
    src/rhyme_assertion_to_be_array_2d.f90
    src/rhyme_assertion_to_be_array_scalar.f90
    src/rhyme_assertion_to_be_array.f90
    src/rhyme_assertion_to_be_nan_array.f90
    src/rhyme_assertion_to_be_nan.f90
    src/rhyme_assertion_to_be.f90
    src/rhyme_assertion_expect.f90
    src/rhyme_assertion_describe.f90
    src/rhyme_assertion_reset.f90
    src/rhyme_assertion.f90)

set(rhyme_src_dir ..)
include(${CMAKE_CURRENT_SOURCE_DIR}/${rhyme_src_dir}/../cmake/helper.cmake)
