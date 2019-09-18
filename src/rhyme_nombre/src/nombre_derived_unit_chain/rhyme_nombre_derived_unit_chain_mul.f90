submodule ( rhyme_nombre_derived_unit_chain ) mul_smod
contains
  module function rhyme_nombre_derived_unit_chain_mul_ducduc ( duc1, duc2 ) result ( new_duc )
    implicit none

    type ( nombre_derived_unit_t ), target, intent ( in ) :: duc1, duc2
    type ( nombre_derived_unit_t ), pointer :: new_duc

    type ( nombre_derived_unit_t ), pointer :: duc1clone, duc2clone
    type ( nombre_base_unit_t ), pointer :: u_ptr

    duc1clone => .tail. ( .clonechain. duc1 )
    duc2clone => .head. ( .clonechain. duc2 )

    if ( len_trim( duc1clone%symb ) .eq. 0 .and. len_trim( duc2clone%symb ) .eq. 0 ) then
      u_ptr => .tail. duc1clone%head

      u_ptr%next => .clonechain. ( duc2clone%head**(duc2clone%pow / duc1clone%pow) )
      u_ptr%next%prev => u_ptr

      duc2clone => duc2clone%next
    end if

    if ( associated( duc2clone ) ) then
      duc1clone%next => duc2clone
      duc1clone%next%prev => duc1clone
    end if

    duc1clone%dim = rhyme_nombre_base_unit_chain_get_dim( duc1clone%head )
    new_duc => .head. duc1clone
  end function rhyme_nombre_derived_unit_chain_mul_ducduc


  module function rhyme_nombre_derived_unit_chain_mul_ducbuc ( duc, buc ) result ( new_duc )
    implicit none

    type ( nombre_derived_unit_t ), target, intent ( in ) :: duc
    type ( nombre_base_unit_t ), target, intent ( in ) :: buc
    type ( nombre_derived_unit_t ), pointer :: new_duc

    type ( nombre_derived_unit_t ), pointer :: new_duc_tail
    type ( nombre_base_unit_t ), pointer :: unit_tail

    new_duc_tail => .tail. ( .clonechain. duc )

    if ( len_trim( new_duc_tail%symb ) .eq. 0 ) then
      unit_tail => .tail. new_duc_tail%head
      unit_tail%next => .clonechain. ( buc**(1d0 / new_duc_tail%pow) )
      unit_tail%next%prev => unit_tail
    else
      new_duc_tail%next => rhyme_nombre_derived_unit_new()
      new_duc_tail%next%prev => new_duc_tail
      new_duc_tail => new_duc_tail%next

      new_duc_tail%head => .clonechain. buc
    end if

    new_duc_tail%dim = rhyme_nombre_base_unit_chain_get_dim( new_duc_tail%head )
    new_duc => .head. new_duc_tail
  end function rhyme_nombre_derived_unit_chain_mul_ducbuc


  module function rhyme_nombre_derived_unit_chain_mul_bucduc ( buc, duc ) result ( new_duc )
    implicit none

    type ( nombre_base_unit_t ), target, intent ( in ) :: buc
    type ( nombre_derived_unit_t ), target, intent ( in ) :: duc
    type ( nombre_derived_unit_t ), pointer :: new_duc

    type ( nombre_derived_unit_t ), pointer :: new_duc_head
    type ( nombre_base_unit_t ), pointer :: unit_head, unit_tail

    new_duc_head => .head. ( .clonechain. duc )

    if ( len_trim( new_duc_head%symb ) .eq. 0 ) then
      unit_head => .head. ( .clonechain. new_duc_head%head )

      new_duc_head%head => .clonechain. ( buc**(1d0 / new_duc_head%pow) )
      unit_tail => .tail. new_duc_head%head
      unit_tail%next => unit_head
      unit_tail%next%prev => unit_tail
    else
      new_duc_head%prev => rhyme_nombre_derived_unit_new()
      new_duc_head%prev%next => new_duc_head
      new_duc_head => new_duc_head%prev

      new_duc_head%head => .clonechain. buc
    end if

    new_duc_head%dim = rhyme_nombre_base_unit_chain_get_dim( new_duc_head%head )
    new_duc => .head. new_duc_head
  end function rhyme_nombre_derived_unit_chain_mul_bucduc


  module function rhyme_nombre_derived_unit_chain_mul_iduc ( i, duc ) result ( new_duc )
    implicit none

    integer, intent ( in ) :: i
    type ( nombre_derived_unit_t ), target, intent ( in ) :: duc
    type ( nombre_derived_unit_t ), pointer :: new_duc

    type ( nombre_derived_unit_t ), pointer :: du_ptr

    new_duc => .clonechain. duc
    du_ptr => .head. new_duc

    do while ( associated( du_ptr ) )
      du_ptr%conv = i * du_ptr%conv
      du_ptr => du_ptr%next
    end do
  end function rhyme_nombre_derived_unit_chain_mul_iduc


  module function rhyme_nombre_derived_unit_chain_mul_rduc ( r, duc ) result ( new_duc )
    implicit none

    real ( kind=4 ), intent ( in ) :: r
    type ( nombre_derived_unit_t ), target, intent ( in ) :: duc
    type ( nombre_derived_unit_t ), pointer :: new_duc

    type ( nombre_derived_unit_t ), pointer :: du_ptr

    new_duc => .clonechain. duc
    du_ptr => .head. new_duc

    do while ( associated( du_ptr ) )
      du_ptr%conv = real( r, kind=8 ) * du_ptr%conv
      du_ptr => du_ptr%next
    end do
  end function rhyme_nombre_derived_unit_chain_mul_rduc


  module function rhyme_nombre_derived_unit_chain_mul_r8duc ( r8, duc ) result ( new_duc )
    implicit none

    real ( kind=8 ), intent ( in ) :: r8
    type ( nombre_derived_unit_t ), target, intent ( in ) :: duc
    type ( nombre_derived_unit_t ), pointer :: new_duc

    type ( nombre_derived_unit_t ), pointer :: du_ptr

    new_duc => .clonechain. duc
    du_ptr => .head. new_duc

    do while ( associated( du_ptr ) )
      du_ptr%conv = r8 * du_ptr%conv
      du_ptr => du_ptr%next
    end do
  end function rhyme_nombre_derived_unit_chain_mul_r8duc


  module function rhyme_nombre_derived_unit_chain_mul_pduc ( p, duc ) result ( new_duc )
    implicit none

    type ( nombre_prefix_t ), intent ( in ) :: p
    type ( nombre_derived_unit_t ), target, intent ( in ) :: duc
    type ( nombre_derived_unit_t ), pointer :: new_duc

    type ( nombre_derived_unit_t ), pointer :: du_ptr

    new_duc => .clonechain. duc
    du_ptr => .head. new_duc

    do while ( associated( du_ptr ) )
      du_ptr%prefix = p * du_ptr%prefix
      du_ptr => du_ptr%next
    end do
  end function rhyme_nombre_derived_unit_chain_mul_pduc
end submodule mul_smod
