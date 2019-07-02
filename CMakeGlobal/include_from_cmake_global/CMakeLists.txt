function( include_from_cmake_global filename )
  include( ${CMAKE_CURRENT_SOURCE_DIR}/${rhyme_src_dir}/../CMakeGlobal/${filename}/CMakeLists.txt )
endfunction()
