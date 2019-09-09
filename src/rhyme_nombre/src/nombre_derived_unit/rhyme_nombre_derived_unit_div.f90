submodule ( rhyme_nombre_derived_unit ) div_smod
contains
  module function rhyme_nombre_derived_unit_div_uu ( u1, u2 ) result ( chain )
    implicit none

    type ( nombre_base_unit_t ), intent ( in ) :: u1, u2
    type ( nombre_derived_unit_t ), pointer :: chain

    chain => rhyme_nombre_derived_unit_new()
    chain%head => rhyme_nombre_base_unit_clone( u1 )
    chain%head%next => rhyme_nombre_base_unit_clone( u2**(-1) )
    chain%head%next%prev => chain%head

    chain%dim = rhyme_nombre_derived_unit_get_dim( chain )
  end function rhyme_nombre_derived_unit_div_uu

  module function rhyme_nombre_derived_unit_div_cu ( dunit, u ) result ( chain )
    implicit none

    type ( nombre_derived_unit_t ), intent ( in ) :: dunit
    type ( nombre_base_unit_t ), intent ( in ) :: u
    type ( nombre_derived_unit_t ), pointer :: chain

    type ( nombre_base_unit_t ), pointer :: tail

    chain => rhyme_nombre_derived_unit_clone( dunit )
    tail => rhyme_nombre_base_unit_tail( chain%head )

    tail%next => rhyme_nombre_base_unit_clone( u**(-1d0/chain%pow))
    tail%next%prev => tail

    chain%dim = rhyme_nombre_derived_unit_get_dim( chain )
  end function rhyme_nombre_derived_unit_div_cu

  module function rhyme_nombre_derived_unit_div_iu ( i, u ) result ( chain )
    implicit none

    integer, intent ( in ) :: i
    type ( nombre_base_unit_t ), intent ( in ) :: u
    type ( nombre_derived_unit_t ), pointer :: chain

    chain => rhyme_nombre_derived_unit_new()
    chain%head => rhyme_nombre_base_unit_clone( u**(-1) )

    chain%conv = chain%conv * i

    chain%dim = rhyme_nombre_derived_unit_get_dim( chain )
  end function rhyme_nombre_derived_unit_div_iu

  module function rhyme_nombre_derived_unit_div_ru ( r, u ) result ( chain )
    implicit none

    real ( kind=4 ), intent ( in ) :: r
    type ( nombre_base_unit_t ), intent ( in ) :: u
    type ( nombre_derived_unit_t ), pointer :: chain

    chain => rhyme_nombre_derived_unit_new()
    chain%head => rhyme_nombre_base_unit_clone( u**(-1) )

    chain%conv = chain%conv * real( r, kind=8 )

    chain%dim = rhyme_nombre_derived_unit_get_dim( chain )
  end function rhyme_nombre_derived_unit_div_ru

  module function rhyme_nombre_derived_unit_div_r8u ( r8, u ) result ( chain )
    implicit none

    real ( kind=8 ), intent ( in ) :: r8
    type ( nombre_base_unit_t ), intent ( in ) :: u
    type ( nombre_derived_unit_t ), pointer :: chain

    chain => rhyme_nombre_derived_unit_new()
    chain%head => rhyme_nombre_base_unit_clone( u**(-1) )

    chain%conv = chain%conv * r8

    chain%dim = rhyme_nombre_derived_unit_get_dim( chain )
  end function rhyme_nombre_derived_unit_div_r8u
end submodule div_smod
