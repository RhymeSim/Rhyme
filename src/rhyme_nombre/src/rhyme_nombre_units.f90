module rhyme_nombre_units
  use rhyme_nombre_unit

  implicit none

  type ( nombre_unit_t ), dimension ( 13 ), parameter :: nombre_units_chain = [ &
    nombre_unit_t( one, "m", 1.d0, LengthDim ), &
    nombre_unit_t( kilo, "g", 1.d0, MassDim ), &
    nombre_unit_t( one, "s", 1.d0, TimeDim ), &
    nombre_unit_t( one, "K", 1.d0, TemperatureDim ), &
    nombre_unit_t( one, "A", 1.d0, ElectricCurrentDim ), &
    nombre_unit_t( one, "mol", 1.d0, AmountOfSubstanceDim ), &
    nombre_unit_t( one, "pc", 3.086d16, LengthDim ), &
    nombre_unit_t( one, "ly", 9.461d15, LengthDim ), &
    nombre_unit_t( one, "AU", 1.496d11, LengthDim ), &
    nombre_unit_t( one, "Msun", 1.9885d33, MassDim ), &
    nombre_unit_t( one, "yr", 3.154d7, TimeDim ), &
    nombre_unit_t( one, "g", 1.d0, MassDim ), &
    nombre_unit_t( one, "m_H", 1.6735575d-24, MassDim ) &
   ]


  type( nombre_unit_t ), target :: meter = nombre_units_chain(1)
  type( nombre_unit_t ), target :: kg = nombre_units_chain(2)
  type( nombre_unit_t ), target :: sec = nombre_units_chain(3)
  type( nombre_unit_t ), target :: kel = nombre_units_chain(4)
  type( nombre_unit_t ), target :: Ampere = nombre_units_chain(5)
  type( nombre_unit_t ), target :: mol = nombre_units_chain(6)
  type( nombre_unit_t ), target :: pc = nombre_units_chain(7)
  type( nombre_unit_t ), target :: ly = nombre_units_chain(8)
  type( nombre_unit_t ), target :: au = nombre_units_chain(9)
  type( nombre_unit_t ), target :: m_sun = nombre_units_chain(10)
  type( nombre_unit_t ), target :: yr = nombre_units_chain(11)
  type( nombre_unit_t ), target :: gram = nombre_units_chain(12)
  type( nombre_unit_t ), target :: m_H = nombre_units_chain(13)


  interface rhyme_nombre_units_handle_pow
    procedure rhyme_nombre_units_handle_pow_int
    procedure rhyme_nombre_units_handle_pow_real
    procedure rhyme_nombre_units_handle_pow_real8
  end interface rhyme_nombre_units_handle_pow

  interface operator ( .unitEqualsTo. )
    procedure rhyme_nombre_units_unit_is_equal_to
  end interface operator ( .unitEqualsTo. )

contains

  function nombre_units_parse ( str ) result ( u )
    implicit none

    character ( len=256 ), intent ( in ) :: str
    type ( nombre_unit_t ), pointer :: u

    character ( len=8 ), dimension ( 32 ) :: arr


    u => null()

    arr = str_unit_tokenizer( str )

    u => nombre_units_term_parse( arr, 1 )
  end function nombre_units_parse


  recursive function nombre_units_term_parse ( arr, i ) result ( u )
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
        u => nombre_units_term_parse( arr, i+1 )
        i = close_par_pos( arr, i ) + 1
        if ( trim( arr(i) ) .eq. "^" ) i = i + 2

      case ( "^" )
        read( arr(i+1), * ) exponent
        u => u**exponent
        i = i + 2

      case ( "*" )
        if ( trim( arr(i+1) ) .eq. "(" ) then
          u => u * nombre_units_term_parse( arr, i+2 )
          i = close_par_pos( arr, i+1 ) + 1
          if ( trim( arr(i) ) .eq. "^" ) i = i + 2
        else if ( trim( arr(i+2) ) .eq. "^" ) then
          unit_ptr => nombre_units_parse_single( arr(i+1) )
          read( arr(i+3), * ) exponent
          unit_ptr => unit_ptr**exponent
          u => u * unit_ptr
          i = i + 4
        else
          unit_ptr => nombre_units_parse_single( arr(i+1) )
          u => u * unit_ptr
          i = i + 2
        end if

      case ( "/" )
        if ( trim( arr(i+1) ) .eq. "(" ) then
          u => u / nombre_units_term_parse( arr, i+2 )
          i = close_par_pos( arr, i+1 ) + 1
          if ( trim( arr(i) ) .eq. "^" ) i = i + 2
        else if ( trim( arr(i+2) ) .eq. "^" ) then
          unit_ptr => nombre_units_parse_single(arr(i+1))
          read( arr(i+3), * ) exponent
          unit_ptr => unit_ptr**exponent
          u => u / unit_ptr
          i = i + 4
        else
          unit_ptr => nombre_units_parse_single( arr(i+1) )
          u => u / unit_ptr
          i = i + 2
        end if

      case default
        unit_ptr => nombre_units_parse_single( arr(i) )
        u => unit_ptr
        i = i + 1
      end select
    end do
  end function nombre_units_term_parse


  function str_unit_tokenizer ( str ) result ( arr )
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
  end function str_unit_tokenizer


  function close_par_pos ( arr, i ) result ( pntr )
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
  end function close_par_pos


  subroutine handle_mul_div ( u1, u2, op )
    implicit none

    type ( nombre_unit_t ), pointer :: u1, u2
    character, intent ( in ) :: op

    select case( trim(op) )
    case ( "*" )
      u1 => u1 * u2
    case ( "/" )
      u1 => u1 / u2
    end select
  end subroutine handle_mul_div


  subroutine rhyme_nombre_units_handle_pow_int ( u, e )
    implicit none

    type ( nombre_unit_t ), pointer :: u
    integer :: e

    u => u**real( e, kind=8 )
  end subroutine rhyme_nombre_units_handle_pow_int


  subroutine rhyme_nombre_units_handle_pow_real ( u, e )
    implicit none

    type ( nombre_unit_t ), pointer :: u
    real :: e

    u => u**real( e, kind=8 )
  end subroutine rhyme_nombre_units_handle_pow_real


  subroutine rhyme_nombre_units_handle_pow_real8 ( u, e )
    implicit none

    type ( nombre_unit_t ), pointer :: u
    real ( kind=8 ) :: e

    u => u**e
  end subroutine rhyme_nombre_units_handle_pow_real8


  function rhyme_nombre_units_unit_is_equal_to ( u1, u2 ) result ( comp )
    implicit none

    type ( nombre_unit_t ), pointer, intent ( in ) :: u1, u2
    logical :: comp

    comp = .false.

    if ( trim( u1%p()) .eq. trim( u2%p() ) ) comp = .true.
  end function rhyme_nombre_units_unit_is_equal_to


  function nombre_units_find ( symb ) result ( u )
    implicit none

    character ( len=* ), intent ( in ) :: symb
    type ( nombre_unit_t ), pointer :: u

    integer :: i

    do i = 1, size( nombre_units_chain )
      if ( trim( symb ) .eq. trim( nombre_units_chain(i)%symb ) ) then
        if ( trim( nombre_units_chain(i)%symb ) .eq. 'g' ) then
          u => mili * nombre_unit_hard_clone( nombre_units_chain(i) )
        else
          u => nombre_unit_hard_clone( nombre_units_chain(i) )
        end if
        return
      end if
    end do

    u => null()
  end function nombre_units_find


  function nombre_units_parse_single ( symb ) result ( u )
    implicit none

    character ( len=* ), intent ( in ) :: symb
    type ( nombre_unit_t ), pointer :: u

    type ( nombre_prefix_t ) :: prfx
    type ( nombre_unit_t ), pointer :: unit => null()


    ! Check units first
    u => nombre_units_find( trim(symb) )
    if ( associated(u) ) return


    ! Check combination of prefixes and units
    prfx = prefix_find( symb )
    unit => nombre_units_find( symb( len_trim(prfx%symb)+1 : ) )
    if ( .not. associated( unit ) ) return

    u => prfx * unit
  end function nombre_units_parse_single


  function prefix_find ( symb ) result ( p )
    implicit none

    character ( len=* ), intent ( in ) :: symb
    type ( nombre_prefix_t ) :: p

    integer :: i, j, lb(1), ub(1)

    lb = lbound( prfx_si )
    ub = ubound( prfx_si )

    do i = lb(1), ub(1)
      if ( len_trim( prfx_si(i)%symb ) == 0 ) cycle

      if ( trim( prfx_si(i)%symb ) .eq. trim( symb( 1:len_trim(prfx_si(i)%symb) ) ) ) then
        do j = 1, size( nombre_units_chain )
          if ( symb( len_trim(prfx_si(i)%symb)+1: ) .eq. trim( nombre_units_chain(j)%symb ) ) then
            p = prfx_si(i)
            return
          end if
        end do
      end if
    end do

    p = one
  end function prefix_find
end module rhyme_nombre_units
