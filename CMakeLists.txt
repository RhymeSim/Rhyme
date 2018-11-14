cmake_minimum_required(VERSION 3.10)

project(rhyme)
enable_language(Fortran)
add_compile_options(-Wall -Wextra -std=f2008)

add_subdirectory(src/nombre)

set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/include)

add_executable(rhyme src/main.f90)
target_link_libraries(rhyme nombre)

install(TARGETS rhyme EXPORT rhyme-targets DESTINATION bin)
