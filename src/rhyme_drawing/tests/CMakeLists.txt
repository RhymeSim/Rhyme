project( rhyme_drawing_tests )
set( test_subject rhyme_drawing )

set( factories
  rhyme_samr
  rhyme_ideal_gas
  rhyme_hydro_base
  rhyme_thermo_base
  rhyme_log
)

set( test_deps
  rhyme_assertion
)

set( rhyme_src_dir ../.. )
include( ${CMAKE_CURRENT_SOURCE_DIR}/../../../CMakeGlobal/tests/CMakeLists.txt )

# Configs
include( ${CMAKE_CURRENT_SOURCE_DIR}/${rhyme_src_dir}/../CMakeGlobal/configs/CMakeLists.txt )
