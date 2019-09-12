submodule ( rhyme_nombre_base_unit ) print_smod
contains
  pure module function rhyme_nombre_base_unit_print ( bu ) result ( str )
    implicit none

    class ( nombre_base_unit_t ), target, intent ( in ) :: bu
    character ( len=64 ) :: str

    str = trim( bu%prefix%symb )//trim( bu%symb )

    if ( abs( bu%pow - 1 ) < tiny(0.d0) ) return

    if ( abs( int( bu%pow ) - bu%pow ) < epsilon(0.d0) ) then
      write ( str, '(A,A,I0)' ) trim( str ), '^', int( bu%pow )
    else
      write ( str, '(A,A,F0.2)' ) trim( str ), '^', bu%pow
    end if

    str = adjustl( str )
  end function rhyme_nombre_base_unit_print
end submodule print_smod
