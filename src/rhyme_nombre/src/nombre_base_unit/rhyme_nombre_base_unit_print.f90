submodule ( rhyme_nombre_base_unit ) print_smod
contains
  module function rhyme_nombre_base_unit_print ( u ) result ( str )
    implicit none

    class ( nombre_base_unit_t ), target, intent ( in ) :: u
    character ( len=64 ) :: str

    write( str, '(A,A)' ) trim( u%prefix%symb ), trim( u%symb )

    if ( abs( u%pow - 1 ) < tiny(0.d0) ) return

    if ( abs( int( u%pow ) - u%pow ) < epsilon(0.d0) ) then
      write ( str, '(A,A,I0)' ) trim( str ), '^', int( u%pow )
    else
      write ( str, '(A,A,F0.2)' ) trim( str ), '^', u%pow
    end if
  end function rhyme_nombre_base_unit_print
end submodule print_smod
