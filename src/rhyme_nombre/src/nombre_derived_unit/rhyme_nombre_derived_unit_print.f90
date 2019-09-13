submodule ( rhyme_nombre_derived_unit ) print_smod
contains
  module function rhyme_nombre_derived_unit_print ( du ) result ( str )
    implicit none

    type ( nombre_derived_unit_t ), intent ( in ) :: du
    character ( len=64 ) :: str

    type ( nombre_base_unit_t ), pointer :: ptr, updated_u

    str = ''

    if ( len_trim( du%symb ) .eq. 0 ) then
      ptr => du%head

      do while ( associated( ptr ) )
        updated_u => du%prefix * ptr
        write( str, '(A,A,A)' ) trim(str), ' ', trim( .print. updated_u )

        ptr => ptr%next
      end do
    else
      str =  trim( du%prefix%symb )//trim( du%symb )

      if ( abs( du%pow - 1 ) > tiny(0d0) ) then
        if ( abs( du%pow - int( du%pow ) ) < tiny(0d0) ) then
          write( str, '(A,A,I0)' ) trim( str ), '^', int( du%pow )
        else
          write( str, '(A,A,F0.2)' ) trim( str ), '^', du%pow
        end if
      end if
    end if

    str = adjustl( str )
  end function rhyme_nombre_derived_unit_print
end submodule print_smod
