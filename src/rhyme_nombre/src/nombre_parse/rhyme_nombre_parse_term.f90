submodule ( rhyme_nombre_parse ) term_smod
contains
  recursive module function rhyme_nombre_parse_term ( str_arr, i ) result ( du )
    implicit none

    character ( len=8 ), intent ( in ) :: str_arr(:)
    integer, value :: i
    type ( nombre_derived_unit_t ), pointer :: du

    type ( nombre_derived_unit_t ), pointer :: du_tmp

    real ( kind=8 ) :: exponent

    du => null()

    do while ( i <= size( str_arr ) .and. .not. str_arr(i) == char(0) )
      select case ( str_arr( i ) )
      case ( ')' )
        if ( str_arr( i+1 ) .eq. '^' ) then
          read( str_arr( i+2 ), * ) exponent
          du => du**exponent
        end if
        return

      case ( '(' )
        du => rhyme_nombre_parse_term( str_arr, i+1 )
        i = rhyme_nombre_parse_close_par_loc( str_arr, i ) + 1
        if ( str_arr( i ) .eq. '^' ) i = i + 2

      case ( '^' )
        read( str_arr( i+1 ), * ) exponent
        du => du**exponent
        i = i + 2

      case ( '*' )
        if ( str_arr( i+1 ) .eq. '(' ) then
          du => du * rhyme_nombre_parse_term( str_arr, i+2 )
          i = rhyme_nombre_parse_close_par_loc( str_arr, i+1 ) + 1
          if ( str_arr(i) .eq. '^' ) i = i + 2
        else if ( str_arr( i+2 ) .eq. '^' ) then
          du_tmp => rhyme_nombre_parse_single( str_arr( i+1 ) )
          read( str_arr( i+3 ), * ) exponent
          du => du * du_tmp**exponent
          i = i + 4
        else
          du => du * rhyme_nombre_parse_single( str_arr( i+1 ) )
          i = i + 2
        end if

      case ( '/' )
        if ( str_arr( i+1 ) .eq. '(' ) then
          du => du / rhyme_nombre_parse_term( str_arr, i+2 )
          i = rhyme_nombre_parse_close_par_loc( str_arr, i+1 ) + 1
          if ( str_arr(i) .eq. '^' ) i = i + 2

        else if ( str_arr( i+2 ) .eq. '^' )then
          du_tmp => rhyme_nombre_parse_single( str_arr( i+1 ) )
          read( str_arr( i+3 ), * ) exponent
          du_tmp => du_tmp**exponent
          du => du / du_tmp
          i = i + 4

        else
          du_tmp => rhyme_nombre_parse_single( str_arr( i+1 ) )
          du => du / du_tmp
          i = i + 2

        end if

      case default
        du_tmp => rhyme_nombre_parse_single( str_arr( i ) )
        du => du_tmp
        i = i + 1

      end select
    end do

  end function rhyme_nombre_parse_term
end submodule term_smod
