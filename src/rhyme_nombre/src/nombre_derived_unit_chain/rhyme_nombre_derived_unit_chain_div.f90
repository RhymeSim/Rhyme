submodule ( rhyme_nombre_derived_unit_chain ) div_smod
contains
  module function rhyme_nombre_derived_unit_chain_div_ducu ( duc, u ) result ( duc_new )
    implicit none

    type ( nombre_derived_unit_t ), intent ( in ) :: duc
    type ( nombre_base_unit_t ), intent ( in ) :: u
    type ( nombre_derived_unit_t ), pointer :: duc_new

    type ( nombre_derived_unit_t ), pointer :: duc_new_tail
    type ( nombre_base_unit_t ), pointer :: unit_tail

    duc_new => rhyme_nombre_derived_unit_chain_clone( duc )
    duc_new_tail => rhyme_nombre_derived_unit_chain_tail( duc_new )

    if ( len_trim( duc_new_tail%symb ) .eq. 0 ) then
      unit_tail => rhyme_nombre_base_unit_tail( duc_new_tail%head )
      unit_tail%next => rhyme_nombre_base_unit_clone( u**(-1) )
      unit_tail%next%prev => unit_tail
    else
      duc_new_tail%next => rhyme_nombre_derived_unit_new()
      duc_new_tail%next%prev => duc_new_tail
      duc_new_tail => duc_new_tail%next

      duc_new_tail%head => rhyme_nombre_base_unit_clone( u**(-1) )
    end if

    duc_new_tail%dim = rhyme_nombre_derived_unit_get_dim( duc_new_tail )
    duc_new => rhyme_nombre_derived_unit_chain_head( duc_new )
  end function rhyme_nombre_derived_unit_chain_div_ducu
end submodule div_smod
