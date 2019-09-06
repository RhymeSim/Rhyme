submodule ( rhyme_nombre_derived_unit ) pow_smod
contains
  module function rhyme_nombre_derived_unit_pow_ci ( c, i ) result ( new )
    implicit none

    type ( nombre_derived_unit_t ), intent ( in ) :: c
    integer, intent ( in ) :: i
    type ( nombre_derived_unit_t ), pointer :: new

    new => rhyme_nombre_derived_unit_clone( c )
    new%pow = new%pow * i
  end function rhyme_nombre_derived_unit_pow_ci

  module function rhyme_nombre_derived_unit_pow_cr ( c, r ) result ( new )
    implicit none

    type ( nombre_derived_unit_t ), intent ( in ) :: c
    real ( kind=4 ), intent ( in ) :: r
    type ( nombre_derived_unit_t ), pointer :: new

    new => rhyme_nombre_derived_unit_clone( c )
    new%pow = new%pow * real( r, kind=8 )
  end function rhyme_nombre_derived_unit_pow_cr

  module function rhyme_nombre_derived_unit_pow_cr8 ( c, r8 ) result ( new )
    implicit none

    type ( nombre_derived_unit_t ), intent ( in ) :: c
    real ( kind=8 ), intent ( in ) :: r8
    type ( nombre_derived_unit_t ), pointer :: new

    new => rhyme_nombre_derived_unit_clone( c )
    new%pow = new%pow * r8
  end function rhyme_nombre_derived_unit_pow_cr8
end submodule pow_smod
