submodule ( rhyme_nombre_derived_unit ) print_smod
contains
  module function rhyme_nombre_derived_unit_print ( c ) result ( str )
    implicit none

    type ( nombre_derived_unit_t ), intent ( in ) :: c
    character ( len=64 ) :: str

    type ( nombre_base_unit_t ), pointer :: ptr

    str = ''

    if ( trim( c%symb ) .eq. '' ) then
      ptr => c%head

      do while ( associated( ptr ) )
        write( str, '(A,A,A)' ) trim(str), ' ', trim( .print. ptr )
        ptr => ptr%next
      end do
    else
      write( str, '(A,A)' ) trim( c%prefix%symb ), trim( c%symb )

      if ( abs( c%pow - 1 ) < tiny(0d0) ) return

      if ( abs( c%pow - int(c%pow) ) < tiny(0d0) ) then
        write( str, '(A,A,I0)' ) trim( str ), '^', int( c%pow )
      else
        write( str, '(A,A,F0.2)' ) trim( str ), '^', c%pow
      end if
    end if
  end function rhyme_nombre_derived_unit_print
end submodule print_smod
