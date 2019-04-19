project( rhyme_irs_tests )
set( test_subject rhyme_irs )

configure_file(
  "sod_shock_tube_t_0_2_analytical.txt"
  "sod_shock_tube_t_0_2_analytical.txt"
  COPYONLY
)

set( factories
  rhyme_hydro_base
)

set( test_deps )

set( proj_src_dir ../.. )
include( ${CMAKE_CURRENT_SOURCE_DIR}/../../../CMakeGlobal/tests/CMakeLists.txt )
