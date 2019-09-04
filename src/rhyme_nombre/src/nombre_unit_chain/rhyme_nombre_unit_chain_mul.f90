submodule ( rhyme_nombre_unit_chain ) mul_smod
contains
  module function rhyme_nombre_unit_chain_mul_uu ( u1, u2 ) result ( chain )
    implicit none

    type ( nombre_unit_t ), target, intent ( in ) :: u1, u2
    type ( nombre_unit_chain_t ), pointer :: chain

    chain => rhyme_nombre_unit_chain_new()
    chain%head => rhyme_nombre_unit_clone( u1 )
    chain%head%next => rhyme_nombre_unit_clone( u2 )
    chain%head%next%prev => chain%head

    chain%dim = rhyme_nombre_unit_chain_get_dim( chain )
  end function rhyme_nombre_unit_chain_mul_uu

  module function rhyme_nombre_unit_chain_mul_cu ( c, u ) result ( chain )
    implicit none

    type ( nombre_unit_chain_t ), target, intent ( in ) :: c
    type ( nombre_unit_t ), target, intent ( in ) :: u
    type ( nombre_unit_chain_t ), pointer :: chain

    type ( nombre_unit_t ), pointer :: tail

    chain => rhyme_nombre_unit_chain_clone( c )
    tail => rhyme_nombre_unit_tail( chain%head )

    tail%next => rhyme_nombre_unit_clone( u**(1d0/chain%pow) )
    tail%next%prev => tail

    chain%dim = rhyme_nombre_unit_chain_get_dim( chain )
  end function rhyme_nombre_unit_chain_mul_cu

  module function rhyme_nombre_unit_chain_mul_cc ( c1, c2 ) result ( chain )
    implicit none

    type ( nombre_unit_chain_t ), target, intent ( in ) :: c1, c2
    type ( nombre_unit_chain_t ), pointer :: chain

    type ( nombre_unit_chain_t ), pointer :: c1tail, c2head

    ! TODO: clone first
    c1tail => rhyme_nombre_unit_chain_tail( c1 )
    c2head => rhyme_nombre_unit_chain_head( c2 )

    c1tail%next => c2head
    c2head%prev => c1tail

    chain => rhyme_nombre_unit_chain_tail( c2head )
  end function rhyme_nombre_unit_chain_mul_cc
end submodule mul_smod
