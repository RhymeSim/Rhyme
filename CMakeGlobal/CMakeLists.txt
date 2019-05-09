# Set generic compiler flags
add_compile_options( -Wall -Wextra )

# Set Fortran compiler flags
set( CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -std=f2008" )

# Set Include directory
set( INCLUDE_DIR ${CMAKE_BINARY_DIR}/include CACHE PATH "Include directory" )
set( CMAKE_Fortran_MODULE_DIRECTORY ${INCLUDE_DIR} )


# Add dependencies
foreach( dep ${deps} )
  if( NOT TARGET ${dep} )
    add_subdirectory( ${proj_src_dir}/${dep} ${CMAKE_BINARY_DIR}/${dep} )
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
  inotifywait -m -r -e modify -e create -e delete -e move ${PROJECT_SOURCE_DIR} \
  | while read -r filename event; do \
    cmake .. && make -j8; ctest -j8 --output-on-failure --timeout 50; \
  done"
  WORKING_DIRECTORY ${CMAKE_BINARY_DIR}
)
