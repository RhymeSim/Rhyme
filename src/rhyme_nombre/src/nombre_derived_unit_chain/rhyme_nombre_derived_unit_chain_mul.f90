submodule ( rhyme_nombre_derived_unit_chain ) mul_smod
contains
  module function rhyme_nombre_derived_unit_chain_mul_ducduc ( duc1, duc2 ) result ( chain )
    implicit none

    type ( nombre_derived_unit_t ), target, intent ( in ) :: duc1, duc2
    type ( nombre_derived_unit_t ), pointer :: chain

    type ( nombre_derived_unit_t ), pointer :: duc1clone, duc2clone
    type ( nombre_base_unit_t ), pointer :: u_ptr

    duc1clone => .tail. ( .clonechain. duc1 )
    duc2clone => .head. ( .clonechain. duc2 )

    if ( len_trim( duc1clone%symb ) .eq. 0 .and. len_trim( duc2clone%symb ) .eq. 0 ) then
      u_ptr => .tail. duc1clone%head

      u_ptr%next => (.clonechain. duc2clone%head )**(duc2clone%pow / duc1clone%pow)
      u_ptr%next%prev => u_ptr

      duc2clone => duc2clone%next
    end if

    duc1clone%next => duc2clone

    if ( associated( duc2clone ) ) then
      duc1clone%next%prev => duc1clone
    end if

    duc1clone%dim = rhyme_nombre_derived_unit_get_dim( duc1clone )
    chain => .head. duc1clone
  end function rhyme_nombre_derived_unit_chain_mul_ducduc


  module function rhyme_nombre_derived_unit_chain_mul_ducu ( duc, u ) result ( duc_new )
    implicit none

    type ( nombre_derived_unit_t ), target, intent ( in ) :: duc
    type ( nombre_base_unit_t ), target, intent ( in ) :: u
    type ( nombre_derived_unit_t ), pointer :: duc_new

    type ( nombre_derived_unit_t ), pointer :: duc_new_tail
    type ( nombre_base_unit_t ), pointer :: unit_tail

    duc_new => .clonechain. duc
    duc_new_tail => .tail. duc_new

    if ( len_trim( duc_new_tail%symb ) .eq. 0 ) then
      unit_tail => .tail. duc_new_tail%head
      unit_tail%next => .clone. u**( 1d0 / duc_new_tail%pow )
      unit_tail%next%prev => unit_tail
    else
      duc_new_tail%next => rhyme_nombre_derived_unit_new()
      duc_new_tail%next%prev => duc_new_tail
      duc_new_tail => duc_new_tail%next

      duc_new_tail%head => .clone. u
    end if

    duc_new_tail%dim = rhyme_nombre_derived_unit_get_dim( duc_new_tail )
    duc_new => .head. duc_new
  end function rhyme_nombre_derived_unit_chain_mul_ducu


  module function rhyme_nombre_derived_unit_chain_mul_uduc ( u, duc ) result ( duc_new )
    implicit none

    type ( nombre_base_unit_t ), target, intent ( in ) :: u
    type ( nombre_derived_unit_t ), target, intent ( in ) :: duc
    type ( nombre_derived_unit_t ), pointer :: duc_new

    type ( nombre_derived_unit_t ), pointer :: duc_new_head
    type ( nombre_base_unit_t ), pointer :: unit_head

    duc_new => .clonechain. duc
    duc_new_head => .head. duc_new

    if ( len_trim( duc_new_head%symb ) .eq. 0 ) then
      unit_head => .head. duc_new_head%head

      duc_new_head%head => .clone. u**(1d0 / duc_new_head%pow)
      duc_new_head%head%next => unit_head
      duc_new_head%head%next%prev => duc_new_head%head
    else
      duc_new_head%prev => rhyme_nombre_derived_unit_new()
      duc_new_head%prev%next => duc_new_head
      duc_new_head => duc_new_head%prev

      duc_new_head%head => .clone. u
    end if

    duc_new_head%dim = rhyme_nombre_derived_unit_get_dim( duc_new_head )
    duc_new => .head. duc_new
  end function rhyme_nombre_derived_unit_chain_mul_uduc
end submodule mul_smod
