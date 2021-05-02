project(rhyme_deep_rs_tests)
set(test_subject rhyme_deep_rs)

set(factories rhyme_units rhyme_logger)

set(test_deps rhyme_hdf5_util rhyme_assertion)

set(assertions)

configure_file("./../models/v20210502.h5" "v20210502.h5" COPYONLY)

set(rhyme_src_dir ../..)
include(${CMAKE_CURRENT_SOURCE_DIR}/${rhyme_src_dir}/../cmake/test_helper.cmake)

# Configs
include(${CMAKE_CURRENT_SOURCE_DIR}/${rhyme_src_dir}/../config.cmake)
