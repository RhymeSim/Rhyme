submodule ( rhyme_nombre_derived_unit_chain ) mul_smod
contains
  module function rhyme_nombre_derived_unit_chain_mul_cc ( dunit1, dunit2 ) result ( chain )
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
  end function rhyme_nombre_derived_unit_chain_mul_cc
end submodule mul_smod
