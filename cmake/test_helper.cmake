enable_language( C Fortran )

# Set generic compiler flags
if ( "${CMAKE_Fortran_COMPILER_ID}" STREQUAL "Intel" )
  add_compile_options( -warn all -fpp -O2 )
elseif( "${CMAKE_Fortran_COMPILER_ID}" STREQUAL "GNU" )
  add_compile_options( -Wall -Wextra -cpp -O2 )
endif()

# Set Fortran compiler flags
if ( "${CMAKE_Fortran_COMPILER_ID}" STREQUAL "Intel" )
  set( CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS}" )
elseif( "${CMAKE_Fortran_COMPILER_ID}" STREQUAL "GNU" )
  set( CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS}" )
endif()

# Set C compiler flags
if ( "${CMAKE_C_COMPILER_ID}" STREQUAL "Intel" )
  set( CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -stand=c11" )
elseif( "${CMAKE_C_COMPILER_ID}" STREQUAL "GNU" )
  set( CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -std=c11" )
endif()


# Set include directory
set( INCLUDE_DIR ${CMAKE_BINARY_DIR}/include CACHE PATH "Include directory" )
set( CMAKE_Fortran_MODULE_DIRECTORY ${INCLUDE_DIR} )


# Listing test and factory files
set( test_files )
set( factory_files )
set( assertion_files )
file( GLOB_RECURSE test_files RELATIVE ${PROJECT_SOURCE_DIR} *_test.f90 )
file( GLOB_RECURSE factory_files RELATIVE ${PROJECT_SOURCE_DIR} *_factory.f90 )
file( GLOB_RECURSE assertion_files RELATIVE ${PROJECT_SOURCE_DIR} *_assertion.f90 )

# Extracting test filenames
set( test_filenames )
foreach( test_file ${test_files} )
  get_filename_component( test_filename ${test_file} NAME_WE )
  list( APPEND test_filenames ${test_filename}_ )
endforeach( test_file )

create_test_sourcelist( _ test_runner.c ${test_filenames} )


# Adding factory directories
foreach( factory ${factories} )
  list( APPEND factory_files
    ${rhyme_src_dir}/${factory}/tests/${factory}_factory.f90 )

  if( NOT TARGET ${factory} )
    add_subdirectory( ${rhyme_src_dir}/${factory}
      ${CMAKE_BINARY_DIR}/${factory} )
  endif()
endforeach( factory )


# Adding assertion files
set( tmp )
if ( DEFINED assertions )
  foreach( assertion ${assertions} )
    file( GLOB_RECURSE tmp ${rhyme_src_dir}/${assertion}/tests/*_assertion.f90 )
    list( APPEND assertion_files ${tmp} )

    if( NOT TARGET ${assertion} )
      add_subdirectory( ${CMAKE_CURRENT_SOURCE_DIR}/src/${assertion}
      ${CMAKE_BINARY_DIR}/${assertion} )
    endif()
  endforeach()
endif()


# Adding internal factory directories
foreach( factory ${internal_factories} )
  list( APPEND factory_files
  ${CMAKE_SOURCE_DIR}/src/${factory}/tests/${factory}_factory.f90 )

  if( NOT TARGET ${factory} )
    add_subdirectory( ${CMAKE_CURRENT_SOURCE_DIR}/src/${factory}
      ${CMAKE_BINARY_DIR}/${factory} )
  endif()
endforeach( factory )


# Adding test dependencies
foreach( dep ${test_deps} )
  if( NOT TARGET ${dep} )
    add_subdirectory( ${rhyme_src_dir}/${dep} ${CMAKE_BINARY_DIR}/${dep} )
  endif()
endforeach( dep )


# Create a library of test and factory files
add_library( ${PROJECT_NAME}_lib STATIC ${test_files} ${factory_files} ${assertion_files} )
add_dependencies( ${PROJECT_NAME}_lib ${test_subject} ${test_deps} ${factories} ${assertions} )

# Create the test runner
add_executable( ${PROJECT_NAME} test_runner.c )
target_link_libraries( ${PROJECT_NAME} PRIVATE
  ${PROJECT_NAME}_lib ${test_subject} ${factories} ${test_deps}
)

set( idx 0 )
list( LENGTH test_files len )

while( ${len} GREATER ${idx} )
  list( GET test_files ${idx} test_file )
  list( GET test_filenames ${idx} test_filename )

  add_test( NAME ${test_file}
    COMMAND $<TARGET_FILE:${PROJECT_NAME}> ${test_filename} )

  math( EXPR idx "${idx} + 1" )
endwhile()

enable_testing()
