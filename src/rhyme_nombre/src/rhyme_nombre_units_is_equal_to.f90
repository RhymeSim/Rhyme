submodule ( rhyme_nombre_units ) rhyme_nombre_units_is_equal_to_smod
contains

  module function rhyme_nombre_units_is_equal_to ( u1, u2 ) result ( is_equal )
    implicit none

    type ( nombre_unit_t ), pointer, intent ( in ) :: u1, u2
    logical :: is_equal

    is_equal = .false.

    if ( trim( u1%p()) .eq. trim( u2%p() ) ) is_equal = .true.
  end function rhyme_nombre_units_is_equal_to
end submodule rhyme_nombre_units_is_equal_to_smod
