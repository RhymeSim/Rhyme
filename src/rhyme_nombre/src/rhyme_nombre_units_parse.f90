submodule ( rhyme_nombre_units ) rhyme_nombre_units_parse_smod
contains

  module function rhyme_nombre_units_parse ( str ) result ( u )
    implicit none

    character ( len=* ), intent ( in ) :: str
    type ( nombre_unit_t ), pointer :: u

    character ( len=8 ), dimension ( 32 ) :: arr

    u => null()
    arr = rhyme_nombre_units_tokenizer( str )
    u => rhyme_nombre_units_parse_section( arr, 1 )
  end function rhyme_nombre_units_parse


  function rhyme_nombre_units_tokenizer ( str ) result ( arr )
    implicit none

    character ( len=256 ), intent ( in ) :: str
    character ( len=8 ), dimension ( 32 ) :: arr

    integer :: char_i, arr_i, i

    arr_i = 1
    char_i = 1
    arr(:)(:) = char(0)

    do i = 1, len_trim( str )
      if ( str( i:i ) .eq. " " ) cycle

      if ( any( [ "^", "*", "/", "(", ")" ] .eq. str(i:i) ) ) then
        if ( .not. arr(arr_i) .eq. char(0) ) arr_i = arr_i + 1

        arr( arr_i ) = str( i:i )
        arr_i = arr_i + 1
        char_i = 1
      else
        arr( arr_i )( char_i:char_i ) = str( i:i )
        char_i = char_i + 1
      end if
    end do
  end function rhyme_nombre_units_tokenizer


  recursive function rhyme_nombre_units_parse_section ( arr, i ) result ( u )
    implicit none

    character ( len=8 ), intent ( in ) :: arr(:)
    type ( nombre_unit_t ), pointer :: u

    type ( nombre_unit_t ), pointer :: unit_ptr
    integer, value :: i

    real ( kind=8 ) :: exponent

    u => null()

    do while ( i <= size( arr ) .and. .not. arr(i) == char(0) )
      select case ( trim( arr(i) ) )

      case ( ")" )
        if ( trim( arr(i+1) ) .eq. "^" ) then
          read( arr(i+2), * ) exponent
          u => u**exponent
        end if

        return

      case ( "(" )
        u => rhyme_nombre_units_parse_section( arr, i+1 )
        i = rhyme_nombre_units_loc_close_par( arr, i ) + 1
        if ( trim( arr(i) ) .eq. "^" ) i = i + 2

      case ( "^" )
        read( arr(i+1), * ) exponent
        u => u**exponent
        i = i + 2

      case ( "*" )
        if ( trim( arr(i+1) ) .eq. "(" ) then
          u => u * rhyme_nombre_units_parse_section( arr, i+2 )
          i = rhyme_nombre_units_loc_close_par( arr, i+1 ) + 1
          if ( trim( arr(i) ) .eq. "^" ) i = i + 2
        else if ( trim( arr(i+2) ) .eq. "^" ) then
          unit_ptr => rhyme_nombre_units_parse_single_term( arr(i+1) )
          read( arr(i+3), * ) exponent
          unit_ptr => unit_ptr**exponent
          u => u * unit_ptr
          i = i + 4
        else
          unit_ptr => rhyme_nombre_units_parse_single_term( arr(i+1) )
          u => u * unit_ptr
          i = i + 2
        end if

      case ( "/" )
        if ( trim( arr(i+1) ) .eq. "(" ) then
          u => u / rhyme_nombre_units_parse_section( arr, i+2 )
          i = rhyme_nombre_units_loc_close_par( arr, i+1 ) + 1
          if ( trim( arr(i) ) .eq. "^" ) i = i + 2
        else if ( trim( arr(i+2) ) .eq. "^" ) then
          unit_ptr => rhyme_nombre_units_parse_single_term(arr(i+1))
          read( arr(i+3), * ) exponent
          unit_ptr => unit_ptr**exponent
          u => u / unit_ptr
          i = i + 4
        else
          unit_ptr => rhyme_nombre_units_parse_single_term( arr(i+1) )
          u => u / unit_ptr
          i = i + 2
        end if

      case default
        unit_ptr => rhyme_nombre_units_parse_single_term( arr(i) )
        u => unit_ptr
        i = i + 1
      end select
    end do
  end function rhyme_nombre_units_parse_section

  function rhyme_nombre_units_loc_close_par ( arr, i ) result ( pntr )
    implicit none

    character ( len=8 ), dimension ( 32 ), intent ( in ) :: arr
    integer, intent ( in ) :: i
    integer :: pntr

    integer :: idx, j

    idx = i
    pntr = 0
    j = 0

    if ( .not. arr( idx ) .eq. "(" ) return

    do while ( idx <= size( arr ) )

      select case ( trim( arr(idx) ) )
      case ( "(" )
        j = j + 1
      case ( ")" )
        j = j - 1
        if ( j .eq. 0 ) then
          pntr = idx
          return
        end if
      end select

      idx = idx + 1
    end do
  end function rhyme_nombre_units_loc_close_par


end submodule rhyme_nombre_units_parse_smod
