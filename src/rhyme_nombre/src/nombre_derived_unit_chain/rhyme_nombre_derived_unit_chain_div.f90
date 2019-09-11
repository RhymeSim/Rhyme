submodule ( rhyme_nombre_derived_unit_chain ) div_smod
contains
  module function rhyme_nombre_derived_unit_chain_div_ducduc ( duc1, duc2 ) result ( ducduc )
    implicit none

    type ( nombre_derived_unit_t ), intent ( in ) :: duc1, duc2
    type ( nombre_derived_unit_t ), pointer :: ducduc

    type ( nombre_derived_unit_t ), pointer :: duc1clone, duc2clone
    type ( nombre_base_unit_t ), pointer :: u_ptr

    duc1clone => .tail. ( duc1**1 )
    duc2clone => .head. ( duc2**(-1) )

    if ( len_trim( duc1clone%symb ) .eq. 0 .and. len_trim( duc2clone%symb ) .eq. 0 ) then
      u_ptr => .tail. duc1clone%head

      u_ptr%next => (.clonechain. duc2clone%head)**(duc2clone%pow / duc1clone%pow)
      u_ptr%next%prev => u_ptr

      duc2clone => duc2clone%next
    end if

    duc1clone%next => duc2clone

    if ( associated( duc2clone ) ) then
      duc1clone%next%prev => duc1clone
    end if

    duc1clone%dim = rhyme_nombre_derived_unit_get_dim( duc1clone )
    ducduc => .head. duc1clone
  end function rhyme_nombre_derived_unit_chain_div_ducduc

  module function rhyme_nombre_derived_unit_chain_div_ducu ( duc, u ) result ( duc_new )
    implicit none

    type ( nombre_derived_unit_t ), intent ( in ) :: duc
    type ( nombre_base_unit_t ), intent ( in ) :: u
    type ( nombre_derived_unit_t ), pointer :: duc_new

    type ( nombre_derived_unit_t ), pointer :: duc_new_tail
    type ( nombre_base_unit_t ), pointer :: unit_tail

    duc_new => .clonechain. duc
    duc_new_tail => .tail. duc_new

    if ( len_trim( duc_new_tail%symb ) .eq. 0 ) then
      unit_tail => .tail. duc_new_tail%head
      unit_tail%next => .clone. u**( -duc_new_tail%pow )
      unit_tail%next%prev => unit_tail
    else
      duc_new_tail%next => rhyme_nombre_derived_unit_new()
      duc_new_tail%next%prev => duc_new_tail
      duc_new_tail => duc_new_tail%next

      duc_new_tail%head => .clone. u**(-1)
    end if

    duc_new_tail%dim = rhyme_nombre_derived_unit_get_dim( duc_new_tail )
    duc_new => .head. duc_new_tail
  end function rhyme_nombre_derived_unit_chain_div_ducu

  module function rhyme_nombre_derived_unit_chain_div_uduc ( u, duc ) result ( duc_new )
    implicit none

    type ( nombre_base_unit_t ), intent ( in ) :: u
    type ( nombre_derived_unit_t ), intent ( in ) :: duc
    type ( nombre_derived_unit_t ), pointer :: duc_new

    type ( nombre_derived_unit_t ), pointer :: duc_new_head, duc_ptr
    type ( nombre_base_unit_t ), pointer :: unit_head, unit_ptr


    duc_new => .clonechain. duc
    duc_new_head => .head. duc_new

    if ( len_trim( duc_new_head%symb ) .eq. 0 ) then
      unit_head => .head. duc_new_head%head

      duc_new_head%head => .clone. u**(1 / duc_new_head%pow)
      duc_new_head%head%next => unit_head
      duc_new_head%head%next%prev => duc_new_head%head

      unit_ptr => duc_new_head%head%next
      do while ( associated( unit_ptr ) )
        unit_ptr%pow = -unit_ptr%pow
        unit_ptr => unit_ptr%next
      end do
    else
      duc_new_head%prev => rhyme_nombre_derived_unit_new()
      duc_new_head%prev%next => duc_new_head
      duc_new_head => duc_new_head%prev

      duc_new_head%head => .clone. u
    end if

    duc_ptr => duc_new_head%next
    do while ( associated( duc_ptr ) )
      duc_ptr%pow = -duc_ptr%pow
      duc_ptr => duc_ptr%next
    end do

    duc_new_head%dim = rhyme_nombre_derived_unit_get_dim( duc_new_head )
    duc_new => .head. duc_new_head
  end function rhyme_nombre_derived_unit_chain_div_uduc
end submodule div_smod
