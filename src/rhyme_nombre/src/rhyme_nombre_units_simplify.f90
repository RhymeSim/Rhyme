submodule ( rhyme_nombre_units ) simplify_smod
contains
  module subroutine rhyme_nombre_units_simplify ( u )
    implicit none

    type ( nombre_unit_t ), pointer, intent ( inout ) :: u

    type ( nombre_unit_t ), pointer :: ptr1, ptr2, next, prev

    ptr1 => rhyme_nombre_units_head( u )

    do while ( associated( ptr1 ) )
      ptr2 => ptr1%next

      do while ( associated( ptr2 ) )
        if ( trim( ptr1%symb ) .eq. trim( ptr2%symb ) ) then
          ptr1%pow = ptr1%pow + ptr2%pow
          ptr1%conv = ptr1%conv * 10**(ptr2%prefix%base_10 - ptr1%prefix%base_10) * ptr2%conv

          next => ptr2%next
          prev => ptr2%prev

          if ( associated( prev ) ) prev%next => next
          if ( associated( next ) ) next%prev => prev

          ptr2 => next
        else
          ptr2 => ptr2%next
        end if
      end do

      ptr1 => ptr1%next
    end do
  end subroutine rhyme_nombre_units_simplify
end submodule simplify_smod
