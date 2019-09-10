submodule ( rhyme_nombre_derived_unit ) div_smod
contains
  module function rhyme_nombre_derived_unit_div_uu ( u1, u2 ) result ( dunit_new )
    implicit none

    type ( nombre_base_unit_t ), intent ( in ) :: u1, u2
    type ( nombre_derived_unit_t ), pointer :: dunit_new

    dunit_new => rhyme_nombre_derived_unit_new()
    dunit_new%head => rhyme_nombre_base_unit_clone( u1 )
    dunit_new%head%next => rhyme_nombre_base_unit_clone( u2**(-1) )
    dunit_new%head%next%prev => dunit_new%head

    dunit_new%dim = rhyme_nombre_derived_unit_get_dim( dunit_new )
  end function rhyme_nombre_derived_unit_div_uu

  module function rhyme_nombre_derived_unit_div_iu ( i, u ) result ( dunit_new )
    implicit none

    integer, intent ( in ) :: i
    type ( nombre_base_unit_t ), intent ( in ) :: u
    type ( nombre_derived_unit_t ), pointer :: dunit_new

    dunit_new => rhyme_nombre_derived_unit_new()
    dunit_new%head => rhyme_nombre_base_unit_clone( u**(-1) )

    dunit_new%conv = dunit_new%conv * i

    dunit_new%dim = rhyme_nombre_derived_unit_get_dim( dunit_new )
  end function rhyme_nombre_derived_unit_div_iu

  module function rhyme_nombre_derived_unit_div_ru ( r, u ) result ( dunit_new )
    implicit none

    real ( kind=4 ), intent ( in ) :: r
    type ( nombre_base_unit_t ), intent ( in ) :: u
    type ( nombre_derived_unit_t ), pointer :: dunit_new

    dunit_new => rhyme_nombre_derived_unit_new()
    dunit_new%head => rhyme_nombre_base_unit_clone( u**(-1) )

    dunit_new%conv = dunit_new%conv * real( r, kind=8 )

    dunit_new%dim = rhyme_nombre_derived_unit_get_dim( dunit_new )
  end function rhyme_nombre_derived_unit_div_ru

  module function rhyme_nombre_derived_unit_div_r8u ( r8, u ) result ( dunit_new )
    implicit none

    real ( kind=8 ), intent ( in ) :: r8
    type ( nombre_base_unit_t ), intent ( in ) :: u
    type ( nombre_derived_unit_t ), pointer :: dunit_new

    dunit_new => rhyme_nombre_derived_unit_new()
    dunit_new%head => rhyme_nombre_base_unit_clone( u**(-1) )

    dunit_new%conv = dunit_new%conv * r8

    dunit_new%dim = rhyme_nombre_derived_unit_get_dim( dunit_new )
  end function rhyme_nombre_derived_unit_div_r8u
end submodule div_smod
