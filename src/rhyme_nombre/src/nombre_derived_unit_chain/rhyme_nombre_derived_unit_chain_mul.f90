submodule ( rhyme_nombre_derived_unit_chain ) mul_smod
contains
  module function rhyme_nombre_derived_unit_chain_mul_ducduc ( duc1, duc2 ) result ( chain )
    implicit none

    type ( nombre_derived_unit_t ), target, intent ( in ) :: duc1, duc2
    type ( nombre_derived_unit_t ), pointer :: chain

    type ( nombre_derived_unit_t ), pointer :: c1clone_tail, c2clone_head

    c1clone_tail => rhyme_nombre_derived_unit_chain_tail( &
      rhyme_nombre_derived_unit_chain_clone( duc1 ) )

    c2clone_head => rhyme_nombre_derived_unit_chain_head( &
      rhyme_nombre_derived_unit_chain_clone( duc2 ) )

    c1clone_tail%next => c2clone_head
    c2clone_head%prev => c1clone_tail

    chain => rhyme_nombre_derived_unit_chain_head( c2clone_head )
  end function rhyme_nombre_derived_unit_chain_mul_ducduc


  module function rhyme_nombre_derived_unit_chain_mul_ducu ( duc, u ) result ( duc_new )
    implicit none

    type ( nombre_derived_unit_t ), target, intent ( in ) :: duc
    type ( nombre_base_unit_t ), target, intent ( in ) :: u
    type ( nombre_derived_unit_t ), pointer :: duc_new

    type ( nombre_derived_unit_t ), pointer :: duc_new_tail
    type ( nombre_base_unit_t ), pointer :: unit_tail

    duc_new => rhyme_nombre_derived_unit_chain_clone( duc )
    duc_new_tail => rhyme_nombre_derived_unit_chain_tail( duc_new )

    if ( len_trim( duc_new_tail%symb ) .eq. 0 ) then
      unit_tail => rhyme_nombre_base_unit_tail( duc_new_tail%head )
      unit_tail%next => rhyme_nombre_base_unit_clone( u )
      unit_tail%next%prev => unit_tail
    else
      duc_new_tail%next => rhyme_nombre_derived_unit_new()
      duc_new_tail%next%prev => duc_new_tail
      duc_new_tail => duc_new_tail%next

      duc_new_tail%head => rhyme_nombre_base_unit_clone( u )
    end if

    duc_new_tail%dim = rhyme_nombre_derived_unit_get_dim( duc_new_tail )
    duc_new => rhyme_nombre_derived_unit_chain_head( duc_new )
  end function rhyme_nombre_derived_unit_chain_mul_ducu


  module function rhyme_nombre_derived_unit_chain_mul_uduc ( u, duc ) result ( duc_new )
    implicit none

    type ( nombre_base_unit_t ), target, intent ( in ) :: u
    type ( nombre_derived_unit_t ), target, intent ( in ) :: duc
    type ( nombre_derived_unit_t ), pointer :: duc_new

    type ( nombre_derived_unit_t ), pointer :: duc_new_head
    type ( nombre_base_unit_t ), pointer :: unit_head

    duc_new => rhyme_nombre_derived_unit_chain_clone( duc )
    duc_new_head => rhyme_nombre_derived_unit_chain_head( duc_new )

    if ( len_trim( duc_new_head%symb ) .eq. 0 ) then
      unit_head => rhyme_nombre_base_unit_head( duc_new_head%head )

      duc_new_head%head => rhyme_nombre_base_unit_clone( u )
      duc_new_head%head%next => unit_head
      duc_new_head%head%next%prev => duc_new_head%head
    else
      duc_new_head%prev => rhyme_nombre_derived_unit_new()
      duc_new_head%prev%next => duc_new_head
      duc_new_head => duc_new_head%prev

      duc_new_head%head => rhyme_nombre_base_unit_clone( u )
    end if

    duc_new_head%dim = rhyme_nombre_derived_unit_get_dim( duc_new_head )
    duc_new => rhyme_nombre_derived_unit_chain_head( duc_new )
  end function rhyme_nombre_derived_unit_chain_mul_uduc
end submodule mul_smod
