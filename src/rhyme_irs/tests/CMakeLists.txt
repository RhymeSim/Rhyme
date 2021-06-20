project(rhyme_irs_tests)
set(test_subject rhyme_irs)

configure_file("assets/p_stars.txt" "p_stars.txt" COPYONLY)

set(factories rhyme_units rhyme_hydro_base rhyme_thermo_base rhyme_logger)

set(test_deps rhyme_assertion)

set(rhyme_src_dir ../..)
include(${CMAKE_CURRENT_SOURCE_DIR}/${rhyme_src_dir}/../cmake/test_helper.cmake)

# Configs
include(${CMAKE_CURRENT_SOURCE_DIR}/${rhyme_src_dir}/../config.cmake)
