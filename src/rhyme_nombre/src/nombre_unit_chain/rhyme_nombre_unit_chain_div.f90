submodule ( rhyme_nombre_unit_chain ) div_smod
contains
  module function rhyme_nombre_unit_chain_div_uu ( u1, u2 ) result ( chain )
    implicit none

    type ( nombre_unit_t ), intent ( in ) :: u1, u2
    type ( nombre_unit_chain_t ), pointer :: chain

    chain => rhyme_nombre_unit_chain_new()
    chain%head => rhyme_nombre_unit_clone( u1 )
    chain%head%next => rhyme_nombre_unit_clone( u2**(-1) )
    chain%head%next%prev => chain%head

    chain%dim = rhyme_nombre_unit_chain_get_dim( chain )
  end function rhyme_nombre_unit_chain_div_uu

  module function rhyme_nombre_unit_chain_div_cu ( c, u ) result ( chain )
    implicit none

    type ( nombre_unit_chain_t ), intent ( in ) :: c
    type ( nombre_unit_t ), intent ( in ) :: u
    type ( nombre_unit_chain_t ), pointer :: chain

    type ( nombre_unit_t ), pointer :: tail

    chain => rhyme_nombre_unit_chain_clone( c )
    tail => rhyme_nombre_unit_tail( chain%head )

    tail%next => rhyme_nombre_unit_clone( u**(-1d0/chain%pow))
    tail%next%prev => tail

    chain%dim = rhyme_nombre_unit_chain_get_dim( chain )
  end function rhyme_nombre_unit_chain_div_cu

  module function rhyme_nombre_unit_chain_div_iu ( i, u ) result ( chain )
    implicit none

    integer, intent ( in ) :: i
    type ( nombre_unit_t ), intent ( in ) :: u
    type ( nombre_unit_chain_t ), pointer :: chain

    chain => rhyme_nombre_unit_chain_new()
    chain%head => rhyme_nombre_unit_clone( u**(-1) )

    chain%conv = chain%conv * i

    chain%dim = rhyme_nombre_unit_chain_get_dim( chain )
  end function rhyme_nombre_unit_chain_div_iu

  module function rhyme_nombre_unit_chain_div_ru ( r, u ) result ( chain )
    implicit none

    real ( kind=4 ), intent ( in ) :: r
    type ( nombre_unit_t ), intent ( in ) :: u
    type ( nombre_unit_chain_t ), pointer :: chain

    chain => rhyme_nombre_unit_chain_new()
    chain%head => rhyme_nombre_unit_clone( u**(-1) )

    chain%conv = chain%conv * real( r, kind=8 )

    chain%dim = rhyme_nombre_unit_chain_get_dim( chain )
  end function rhyme_nombre_unit_chain_div_ru

  module function rhyme_nombre_unit_chain_div_r8u ( r8, u ) result ( chain )
    implicit none

    real ( kind=8 ), intent ( in ) :: r8
    type ( nombre_unit_t ), intent ( in ) :: u
    type ( nombre_unit_chain_t ), pointer :: chain

    chain => rhyme_nombre_unit_chain_new()
    chain%head => rhyme_nombre_unit_clone( u**(-1) )

    chain%conv = chain%conv * r8

    chain%dim = rhyme_nombre_unit_chain_get_dim( chain )
  end function rhyme_nombre_unit_chain_div_r8u
end submodule div_smod
