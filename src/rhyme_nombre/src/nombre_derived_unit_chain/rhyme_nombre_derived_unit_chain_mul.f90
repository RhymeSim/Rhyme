submodule ( rhyme_nombre_derived_unit_chain ) mul_smod
contains
  module function rhyme_nombre_derived_unit_chain_mul_dudu ( dunit1, dunit2 ) result ( chain )
    implicit none

    type ( nombre_derived_unit_t ), target, intent ( in ) :: dunit1, dunit2
    type ( nombre_derived_unit_t ), pointer :: chain

    type ( nombre_derived_unit_t ), pointer :: c1clone_tail, c2clone_head

    c1clone_tail => rhyme_nombre_derived_unit_chain_tail( &
      rhyme_nombre_derived_unit_chain_clone( dunit1 ) )

    c2clone_head => rhyme_nombre_derived_unit_chain_head( &
      rhyme_nombre_derived_unit_chain_clone( dunit2 ) )

    c1clone_tail%next => c2clone_head
    c2clone_head%prev => c1clone_tail

    chain => rhyme_nombre_derived_unit_chain_head( c2clone_head )
  end function rhyme_nombre_derived_unit_chain_mul_dudu


  module function rhyme_nombre_derived_unit_chain_mul_duu ( c, u ) result ( chain )
    implicit none

    type ( nombre_derived_unit_t ), target, intent ( in ) :: c
    type ( nombre_base_unit_t ), target, intent ( in ) :: u
    type ( nombre_derived_unit_t ), pointer :: chain

    type ( nombre_derived_unit_t ), pointer :: chain_tail
    type ( nombre_base_unit_t ), pointer :: unit_tail

    chain => rhyme_nombre_derived_unit_chain_clone( c )
    chain_tail => rhyme_nombre_derived_unit_chain_tail( chain )

    if ( len_trim( chain_tail%symb ) .eq. 0 ) then
      unit_tail => rhyme_nombre_base_unit_tail( chain_tail%head )
      unit_tail%next => rhyme_nombre_base_unit_clone( u )
      unit_tail%next%prev => unit_tail
    else
      chain_tail%next => rhyme_nombre_derived_unit_new()
      chain_tail%next%prev => chain_tail
      chain_tail => chain_tail%next

      chain_tail%head => rhyme_nombre_base_unit_clone( u )
      chain_tail%head%prev => null()
    end if

    chain_tail%dim = rhyme_nombre_derived_unit_get_dim( chain_tail )
    chain => rhyme_nombre_derived_unit_chain_head( chain )
  end function rhyme_nombre_derived_unit_chain_mul_duu


  module function rhyme_nombre_derived_unit_chain_mul_udu ( u, c ) result ( chain )
    implicit none

    type ( nombre_base_unit_t ), target, intent ( in ) :: u
    type ( nombre_derived_unit_t ), target, intent ( in ) :: c
    type ( nombre_derived_unit_t ), pointer :: chain

    type ( nombre_derived_unit_t ), pointer :: chain_head
    type ( nombre_base_unit_t ), pointer :: unit_head

    chain => rhyme_nombre_derived_unit_chain_clone( c )
    chain_head => rhyme_nombre_derived_unit_chain_head( chain )

    if ( len_trim( chain_head%symb ) .eq. 0 ) then
      unit_head => rhyme_nombre_base_unit_head( chain_head%head )

      chain_head%head => rhyme_nombre_base_unit_clone( u )
      chain_head%head%next => unit_head
      chain_head%head%next%prev => chain_head%head
    else
      chain_head%prev => rhyme_nombre_derived_unit_new()
      chain_head%prev%next => chain_head
      chain_head => chain_head%prev

      chain_head%head => rhyme_nombre_base_unit_clone( u )
    end if

    chain_head%dim = rhyme_nombre_derived_unit_get_dim( chain_head )
    chain => rhyme_nombre_derived_unit_chain_head( chain )
  end function rhyme_nombre_derived_unit_chain_mul_udu
end submodule mul_smod
