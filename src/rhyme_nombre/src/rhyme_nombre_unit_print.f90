submodule ( rhyme_nombre_unit ) unit_print_smod
contains
  module function rhyme_nombre_unit_print ( u ) result ( str )
    implicit none

    class ( nombre_unit_t ), target, intent ( in ) :: u
    character ( len=64 ) :: str

    type ( nombre_unit_t ), pointer :: u_head

    str = ''

    u_head => u
    u_head => rhyme_nombre_unit_head( u_head )

    do while ( associated( u_head ) )
      write( str, fmt="(A,A,A,A)" ) trim(str), ' ', trim( u_head%prefix%symb ), trim( u_head%symb )

      if ( abs( u_head%pow - 1 ) > epsilon(0.d0) ) then
        if ( abs( int( u_head%pow ) - u_head%pow ) < epsilon(0.d0) ) then
          write ( str, fmt="(A,A,I0)" ) trim( str ), '^', int( u_head%pow )
        else
          write ( str, fmt="(A,A,F0.1)" ) trim( str ), '^', u_head%pow
        end if
      end if

      u_head => u_head%next
    end do

    str = adjustl( trim( str ) )
  end function rhyme_nombre_unit_print
end submodule unit_print_smod
