submodule ( rhyme_nombre_parse ) string_smod
contains
  module function rhyme_nombre_parse_string ( str ) result ( du )
    implicit none

    character ( len=* ), intent ( in ) :: str
    type ( nombre_derived_unit_t ), pointer :: du

    character ( len=8 ), dimension ( 64 ) :: str_arr

    str_arr = rhyme_nombre_parse_tokenize( str )
    du => parse_term( str_arr, 1 )

    if ( associated( du ) ) du => .head. du
  end function rhyme_nombre_parse_string

  recursive function parse_term ( str_arr, idx ) result ( du )
    implicit none

    character ( len=8 ), intent ( in ) :: str_arr(:)
    integer, intent ( in ) :: idx
    type ( nombre_derived_unit_t ), pointer :: du

    type ( nombre_derived_unit_t ), pointer :: du_tmp
    real ( kind=8 ) :: exponent
    integer :: i

    i = idx
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
        du => parse_term( str_arr, i+1 )
        i = rhyme_nombre_parse_close_par_loc( str_arr, i ) + 1
        if ( str_arr( i ) .eq. '^' ) i = i + 2

      case ( '^' )
        read( str_arr( i+1 ), * ) exponent
        du => du**exponent
        i = i + 2

      case ( '*' )
        if ( str_arr( i+1 ) .eq. '(' ) then
          du => du * parse_term( str_arr, i+2 )
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
          du => du / parse_term( str_arr, i+2 )
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
  end function parse_term
end submodule string_smod
