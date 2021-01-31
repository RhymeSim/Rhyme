project(rhyme_samr_bc_tests)
set(test_subject rhyme_samr_bc)

set(factories rhyme_thermo_base rhyme_samr rhyme_logger)

set(test_deps rhyme_assertion)

set(rhyme_src_dir ../..)
include(${CMAKE_CURRENT_SOURCE_DIR}/${rhyme_src_dir}/../cmake/test_helper.cmake)

# Configs
include(${CMAKE_CURRENT_SOURCE_DIR}/${rhyme_src_dir}/../config.cmake)
