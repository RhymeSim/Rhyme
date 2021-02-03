project(rhyme_sanity_check_tests)
set(test_subject rhyme_sanity_check)

set(factories rhyme_units rhyme_thermo_base rhyme_samr rhyme_logger)

set(test_deps rhyme_units rhyme_samr rhyme_assertion)

set(assertions rhyme_nombre)

set(rhyme_src_dir ../..)
include(${CMAKE_CURRENT_SOURCE_DIR}/${rhyme_src_dir}/../cmake/test_helper.cmake)

# Configs
include(${CMAKE_CURRENT_SOURCE_DIR}/${rhyme_src_dir}/../config.cmake)
