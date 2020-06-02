project(rhyme_units_tests)
set(test_subject rhyme_units)

set(factories rhyme_logger)

set(test_deps rhyme_assertion)

set(assertions rhyme_nombre)

set(rhyme_src_dir ../..)
include(${CMAKE_CURRENT_SOURCE_DIR}/${rhyme_src_dir}/../cmake/test_helper.cmake)

# Configs
include(${CMAKE_CURRENT_SOURCE_DIR}/${rhyme_src_dir}/../config.cmake)
