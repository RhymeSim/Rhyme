submodule ( rhyme_nombre_unit ) print_smod
contains
  module function rhyme_nombre_unit_print ( u ) result ( str )
    implicit none

    class ( nombre_unit_t ), target, intent ( in ) :: u
    character ( len=64 ) :: str

    str = ''

    write( str, fmt="(A,A,A,A)" ) trim(str), ' ', trim( u%prefix%symb ), trim( u%symb )

    if ( abs( u%pow - 1 ) > epsilon(0.d0) ) then
      if ( abs( int( u%pow ) - u%pow ) < epsilon(0.d0) ) then
        write ( str, fmt="(A,A,I0)" ) trim( str ), '^', int( u%pow )
      else
        write ( str, fmt="(A,A,F0.1)" ) trim( str ), '^', u%pow
      end if
    end if

    str = adjustl( trim( str ) )
  end function rhyme_nombre_unit_print
end submodule print_smod
