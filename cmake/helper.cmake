# Set generic compiler flags
if ( "${CMAKE_Fortran_COMPILER_ID}" STREQUAL "Intel" )
  add_compile_options( -warn all -fpp -O3 )
elseif( "${CMAKE_Fortran_COMPILER_ID}" STREQUAL "GNU" )
  add_compile_options( -Wall -Wextra -cpp -O3 )
endif()

# Set Fortran compiler flags
if ( "${CMAKE_Fortran_COMPILER_ID}" STREQUAL "Intel" )
  set( CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS}" )
elseif( "${CMAKE_Fortran_COMPILER_ID}" STREQUAL "GNU" )
  set( CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS}" )
endif()

# Set Include directory
set( INCLUDE_DIR ${CMAKE_BINARY_DIR}/include CACHE PATH "Include directory" )
set( CMAKE_Fortran_MODULE_DIRECTORY ${INCLUDE_DIR} )


# Add dependencies
foreach( dep ${deps} )
  if( NOT TARGET ${dep} )
    add_subdirectory( ${rhyme_src_dir}/${dep} ${CMAKE_BINARY_DIR}/${dep} )
  endif()
endforeach( dep )


# Add internal dependencies
foreach( dep ${internal_deps} )
  if( NOT TARGET ${dep} )
    add_subdirectory( src/${dep} ${CMAKE_BINARY_DIR}/${dep} )
  endif()
endforeach( dep )

# Create a static library
add_library( ${PROJECT_NAME} STATIC ${srcs} )
target_link_libraries( ${PROJECT_NAME} PRIVATE ${deps} ${internal_deps} )

# Enable tests
add_subdirectory( tests )
enable_testing()


# Watch target (Make sure inotify-tools is installed)
add_custom_target( ${PROJECT_NAME}_watch
  VERBATIM COMMAND /bin/sh -c " \
  inotifywait --excludei /*build*/ -m -r -e close_write -e create -e delete -e move ${CMAKE_CURRENT_SOURCE_DIR} \
  | while read -r path action file; do \
    echo \"$path $action $file\"; \
    rm -rf CMakeFiles tests/CMakeFiles; cmake .. && make && ctest --output-on-failure --timeout 3; \
  done"
  WORKING_DIRECTORY ${CMAKE_BINARY_DIR}
)
