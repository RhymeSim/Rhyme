submodule ( rhyme_nombre_derived_unit ) mul_smod
contains
  module function rhyme_nombre_derived_unit_mul_iu ( i, u ) result ( chain )
    implicit none

    integer, intent ( in ) :: i
    type ( nombre_base_unit_t ), target, intent ( in ) :: u
    type ( nombre_derived_unit_t ), pointer :: chain

    chain => rhyme_nombre_derived_unit_new()
    chain%conv = i
    chain%head => rhyme_nombre_base_unit_clone( u )

    chain%dim = rhyme_nombre_derived_unit_get_dim( chain )
  end function rhyme_nombre_derived_unit_mul_iu

  module function rhyme_nombre_derived_unit_mul_ru ( r, u ) result ( chain )
    implicit none

    real ( kind=4 ), intent ( in ) :: r
    type ( nombre_base_unit_t ), target, intent ( in ) :: u
    type ( nombre_derived_unit_t ), pointer :: chain

    chain => rhyme_nombre_derived_unit_new()
    chain%conv = real( r, kind=8 )
    chain%head => rhyme_nombre_base_unit_clone( u )

    chain%dim = rhyme_nombre_derived_unit_get_dim( chain )
  end function rhyme_nombre_derived_unit_mul_ru

  module function rhyme_nombre_derived_unit_mul_r8u ( r8, u ) result ( chain )
    implicit none

    real ( kind=8 ), intent ( in ) :: r8
    type ( nombre_base_unit_t ), target, intent ( in ) :: u
    type ( nombre_derived_unit_t ), pointer :: chain

    chain => rhyme_nombre_derived_unit_new()
    chain%conv = r8
    chain%head => rhyme_nombre_base_unit_clone( u )

    chain%dim = rhyme_nombre_derived_unit_get_dim( chain )
  end function rhyme_nombre_derived_unit_mul_r8u

  module function rhyme_nombre_derived_unit_mul_uu ( u1, u2 ) result ( chain )
    implicit none

    type ( nombre_base_unit_t ), target, intent ( in ) :: u1, u2
    type ( nombre_derived_unit_t ), pointer :: chain

    chain => rhyme_nombre_derived_unit_new()
    chain%head => rhyme_nombre_base_unit_clone( u1 )
    chain%head%next => rhyme_nombre_base_unit_clone( u2 )
    chain%head%next%prev => chain%head

    chain%dim = rhyme_nombre_derived_unit_get_dim( chain )
  end function rhyme_nombre_derived_unit_mul_uu

  module function rhyme_nombre_derived_unit_mul_cu ( dunit, u ) result ( chain )
    implicit none

    type ( nombre_derived_unit_t ), target, intent ( in ) :: dunit
    type ( nombre_base_unit_t ), target, intent ( in ) :: u
    type ( nombre_derived_unit_t ), pointer :: chain

    type ( nombre_base_unit_t ), pointer :: tail

    chain => rhyme_nombre_derived_unit_clone( dunit )
    tail => rhyme_nombre_base_unit_tail( chain%head )

    tail%next => rhyme_nombre_base_unit_clone( u**(1d0/chain%pow) )
    tail%next%prev => tail

    chain%dim = rhyme_nombre_derived_unit_get_dim( chain )
  end function rhyme_nombre_derived_unit_mul_cu

  module function rhyme_nombre_derived_unit_mul_ic ( i, dunit ) result ( chain )
    implicit none

    integer, intent ( in ) :: i
    type ( nombre_derived_unit_t ), target, intent ( in ) :: dunit
    type ( nombre_derived_unit_t ), pointer :: chain

    chain => rhyme_nombre_derived_unit_clone( dunit )
    chain%conv = chain%conv * i
  end function rhyme_nombre_derived_unit_mul_ic

  module function rhyme_nombre_derived_unit_mul_rc ( r, dunit ) result ( chain )
    implicit none

    real ( kind=4 ), intent ( in ) :: r
    type ( nombre_derived_unit_t ), target, intent ( in ) :: dunit
    type ( nombre_derived_unit_t ), pointer :: chain

    chain => rhyme_nombre_derived_unit_clone( dunit )
    chain%conv = chain%conv * real( r, kind=8 )
  end function rhyme_nombre_derived_unit_mul_rc

  module function rhyme_nombre_derived_unit_mul_r8c ( r8, dunit ) result ( chain )
    implicit none

    real ( kind=8 ), intent ( in ) :: r8
    type ( nombre_derived_unit_t ), target, intent ( in ) :: dunit
    type ( nombre_derived_unit_t ), pointer :: chain

    chain => rhyme_nombre_derived_unit_clone( dunit )
    chain%conv = chain%conv * r8
  end function rhyme_nombre_derived_unit_mul_r8c

  module function rhyme_nombre_derived_unit_mul_pc ( p, dunit ) result ( chain )
    implicit none

    type ( nombre_prefix_t ), intent ( in ) :: p
    type ( nombre_derived_unit_t ), target, intent ( in ) :: dunit
    type ( nombre_derived_unit_t ), pointer :: chain

    chain => rhyme_nombre_derived_unit_clone( dunit )
    chain%prefix = p
  end function rhyme_nombre_derived_unit_mul_pc
end submodule mul_smod
