include(
  ${CMAKE_CURRENT_SOURCE_DIR}/${rhyme_src_dir}/../cmake/IfNotDefinedDefine.cmake
)
include(
  ${CMAKE_CURRENT_SOURCE_DIR}/${rhyme_src_dir}/../cmake/CalcNumberOfComponents.cmake
)

set(NUMBER_OF_DIM 3)
set(NUMBER_OF_SPECIES 3)

calc_number_of_components(${NUMBER_OF_DIM} ${NUMBER_OF_SPECIES}
                          NUMBER_OF_COMPONENTS)

if_not_defined_define(NDIM ${NUMBER_OF_DIM})
if_not_defined_define(NSPE ${NUMBER_OF_SPECIES})
if_not_defined_define(NCMP ${NUMBER_OF_COMPONENTS})
