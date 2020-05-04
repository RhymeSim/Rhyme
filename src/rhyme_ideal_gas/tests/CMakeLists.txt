project(rhyme_ideal_gas_tests)
set(test_subject rhyme_ideal_gas)

set(factories rhyme_physics rhyme_hydro_base rhyme_logger)

set(test_deps rhyme_assertion)

set(rhyme_src_dir ../..)
include(${CMAKE_CURRENT_SOURCE_DIR}/${rhyme_src_dir}/../cmake/test_helper.cmake)

# Configs
include(${CMAKE_CURRENT_SOURCE_DIR}/${rhyme_src_dir}/../config.cmake)
