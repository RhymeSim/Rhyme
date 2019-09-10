submodule ( rhyme_nombre_derived_unit ) pow_smod
contains
  module function rhyme_nombre_derived_unit_pow_dui ( du, i ) result ( new_du )
    implicit none

    type ( nombre_derived_unit_t ), intent ( in ) :: du
    integer, intent ( in ) :: i
    type ( nombre_derived_unit_t ), pointer :: new_du

    new_du => rhyme_nombre_derived_unit_clone( du )
    new_du%pow = new_du%pow * i
  end function rhyme_nombre_derived_unit_pow_dui

  module function rhyme_nombre_derived_unit_pow_dur ( du, r ) result ( new_du )
    implicit none

    type ( nombre_derived_unit_t ), intent ( in ) :: du
    real ( kind=4 ), intent ( in ) :: r
    type ( nombre_derived_unit_t ), pointer :: new_du

    new_du => rhyme_nombre_derived_unit_clone( du )
    new_du%pow = new_du%pow * real( r, kind=8)
  end function rhyme_nombre_derived_unit_pow_dur

  module function rhyme_nombre_derived_unit_pow_dur8 ( du, r8 ) result ( new_du )
    implicit none

    type ( nombre_derived_unit_t ), intent ( in ) :: du
    real ( kind=8 ), intent ( in ) :: r8
    type ( nombre_derived_unit_t ), pointer :: new_du

    new_du => rhyme_nombre_derived_unit_clone( du )
    new_du%pow = new_du%pow * r8
  end function rhyme_nombre_derived_unit_pow_dur8
end submodule pow_smod
