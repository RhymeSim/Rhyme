submodule ( rhyme_nombre_dimension ) pow_smod
contains
  pure module function rhyme_nombre_dimension_pow_i ( d, i ) result ( new_d )
    implicit none

    type ( nombre_dimension_t ), intent ( in ) :: d
    integer, intent ( in ) :: i
    type ( nombre_dimension_t ) :: new_d

    new_d%powers = i * d%powers
  end function rhyme_nombre_dimension_pow_i

  pure module function rhyme_nombre_dimension_pow_r ( d, r ) result ( new_d )
    implicit none

    type ( nombre_dimension_t ), intent ( in ) :: d
    real ( kind=4 ), intent ( in ) :: r
    type ( nombre_dimension_t ) :: new_d

    new_d%powers = real( r, kind=8 ) * d%powers
  end function rhyme_nombre_dimension_pow_r

  pure module function rhyme_nombre_dimension_pow_r8 ( d, r8 ) result ( new_d )
    implicit none

    type ( nombre_dimension_t ), intent ( in ) :: d
    real ( kind=8 ), intent ( in ) :: r8
    type ( nombre_dimension_t ) :: new_d

    new_d%powers = r8 * d%powers
  end function rhyme_nombre_dimension_pow_r8
end submodule pow_smod
