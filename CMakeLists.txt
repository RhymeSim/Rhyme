cmake_minimum_required(VERSION 3.10)

project(rhyme)
enable_language(Fortran)
add_compile_options(-Wall -Wextra -std=f2008)

set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/include)


if(NOT TARGET nombre)
  add_subdirectory(src/nombre)
endif()
add_subdirectory(src/chemistry)


add_executable(rhyme src/main.f90)
target_link_libraries(rhyme nombre)
target_link_libraries(rhyme chemistry)

install(TARGETS rhyme EXPORT rhyme-targets DESTINATION bin)
