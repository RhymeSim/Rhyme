submodule ( rhyme_nombre_derived_unit ) print_smod
contains
  module function rhyme_nombre_derived_unit_print ( dunit ) result ( str )
    implicit none

    type ( nombre_derived_unit_t ), intent ( in ) :: dunit
    character ( len=64 ) :: str

    type ( nombre_base_unit_t ), pointer :: ptr, updated_u

    str = ''

    if ( trim( dunit%symb ) .eq. '' ) then
      ptr => dunit%head

      do while ( associated( ptr ) )
        updated_u => dunit%prefix * ptr 
        write( str, '(A,A,A)' ) trim(str), ' ', trim( .print. updated_u )

        ptr => ptr%next
      end do
    else
      write( str, '(A,A)' ) trim( dunit%prefix%symb ), trim( dunit%symb )

      if ( abs( dunit%pow - 1 ) < tiny(0d0) ) return

      if ( abs( dunit%pow - int( dunit%pow ) ) < tiny(0d0) ) then
        write( str, '(A,A,I0)' ) trim( str ), '^', int( dunit%pow )
      else
        write( str, '(A,A,F0.2)' ) trim( str ), '^', dunit%pow
      end if
    end if

    str = adjustl( str )
  end function rhyme_nombre_derived_unit_print
end submodule print_smod
