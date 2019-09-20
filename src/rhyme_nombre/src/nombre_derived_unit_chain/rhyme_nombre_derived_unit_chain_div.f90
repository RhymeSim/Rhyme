submodule ( rhyme_nombre_derived_unit_chain ) div_smod
contains
  module function rhyme_nombre_derived_unit_chain_div_ducduc ( duc1, duc2 ) result ( duc_new )
    implicit none

    type ( nombre_derived_unit_t ), target, intent ( in ) :: duc1, duc2
    type ( nombre_derived_unit_t ), pointer :: duc_new

    type ( nombre_derived_unit_t ), pointer :: duc1clone, duc2clone
    type ( nombre_base_unit_t ), pointer :: u_ptr

    duc1clone => .tail. ( .clonechain. duc1 )
    duc2clone => .head. duc2**(-1)

    if ( len_trim( duc1clone%symb ) .eq. 0 .and. len_trim( duc2clone%symb ) .eq. 0 ) then
      u_ptr => .tail. duc1clone%head

      u_ptr%next => duc2clone%head**(duc2clone%pow / duc1clone%pow)
      u_ptr%next%prev => u_ptr

      duc2clone => duc2clone%next
    end if

    if ( associated( duc2clone ) ) then
      duc1clone%next => duc2clone
      duc1clone%next%prev => duc1clone
    end if

    duc1clone%dim = rhyme_nombre_base_unit_chain_get_dim( duc1clone%head )
    duc_new => .head. duc1clone
  end function rhyme_nombre_derived_unit_chain_div_ducduc


  module function rhyme_nombre_derived_unit_chain_div_ducbuc ( duc, buc ) result ( duc_new )
    implicit none

    type ( nombre_derived_unit_t ), target, intent ( in ) :: duc
    type ( nombre_base_unit_t ), target, intent ( in ) :: buc
    type ( nombre_derived_unit_t ), pointer :: duc_new

    type ( nombre_derived_unit_t ), pointer :: duc_new_tail
    type ( nombre_base_unit_t ), pointer :: unit_tail

    duc_new_tail => .tail. ( .clonechain. duc )

    if ( len_trim( duc_new_tail%symb ) .eq. 0 ) then
      unit_tail => .tail. duc_new_tail%head
      unit_tail%next => buc**( -duc_new_tail%pow )
      unit_tail%next%prev => unit_tail
    else
      duc_new_tail%next => rhyme_nombre_derived_unit_new()
      duc_new_tail%next%prev => duc_new_tail
      duc_new_tail => duc_new_tail%next

      duc_new_tail%head => buc**(-1)
    end if

    duc_new_tail%dim = rhyme_nombre_base_unit_chain_get_dim( duc_new_tail%head )
    duc_new => .head. duc_new_tail
  end function rhyme_nombre_derived_unit_chain_div_ducbuc


  module function rhyme_nombre_derived_unit_chain_div_bucduc ( buc, duc ) result ( duc_new )
    implicit none

    type ( nombre_base_unit_t ), target, intent ( in ) :: buc
    type ( nombre_derived_unit_t ), target, intent ( in ) :: duc
    type ( nombre_derived_unit_t ), pointer :: duc_new

    type ( nombre_derived_unit_t ), pointer :: duc_new_head
    type ( nombre_base_unit_t ), pointer :: unit_head, unit_tail

    duc_new_head => .head. ( duc**(-1) )

    if ( len_trim( duc_new_head%symb ) .eq. 0 ) then
      unit_head => .head. ( .clonechain. duc_new_head%head )
      duc_new_head%head => buc**(-1d0 / duc_new_head%pow)
      unit_tail => .tail. duc_new_head%head
      unit_tail%next => unit_head**(-1)
      unit_tail%next%prev => unit_tail
    else
      duc_new_head%prev => rhyme_nombre_derived_unit_new()
      duc_new_head%prev%next => duc_new_head
      duc_new_head => duc_new_head%prev
      duc_new_head%head => buc**(1d0 / duc_new_head%pow)
    end if

    duc_new_head%dim = rhyme_nombre_base_unit_chain_get_dim( duc_new_head%head )
    duc_new => .head. duc_new_head
  end function rhyme_nombre_derived_unit_chain_div_bucduc
end submodule div_smod
