submodule ( rhyme_nombre_unit ) pow_smod
contains
  module function rhyme_nombre_unit_pow_ui ( u, i ) result ( new )
    implicit none

    type ( nombre_unit_t ), intent ( in ) :: u
    integer, intent ( in ) :: i
    type ( nombre_unit_t ), pointer :: new

    new => rhyme_nombre_unit_clone( u )
    new%pow = new%pow * i
  end function rhyme_nombre_unit_pow_ui

  module function rhyme_nombre_unit_pow_ur ( u, r ) result ( new )
    implicit none

    type ( nombre_unit_t ), intent ( in ) :: u
    real ( kind=4 ), intent ( in ) :: r
    type ( nombre_unit_t ), pointer :: new

    new => rhyme_nombre_unit_clone( u )
    new%pow = new%pow * real( r, kind=8 )
  end function rhyme_nombre_unit_pow_ur

  module function rhyme_nombre_unit_pow_ur8 ( u, r8 ) result ( new )
    implicit none

    type ( nombre_unit_t ), intent ( in ) :: u
    real ( kind=8 ), intent ( in ) :: r8
    type ( nombre_unit_t ), pointer :: new

    new => rhyme_nombre_unit_clone( u )
    new%pow = new%pow * r8
  end function rhyme_nombre_unit_pow_ur8
end submodule pow_smod
