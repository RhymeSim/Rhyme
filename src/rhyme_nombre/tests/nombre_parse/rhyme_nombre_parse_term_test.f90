logical function rhyme_nombre_parse_term_test () result ( failed )
  use rhyme_nombre_parse
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: tester

  real ( kind=8 ) :: u_rnd(5, 2), du_rnd(5, 2) ! prefixs & unit/derived_unit
  integer :: i, upi(5), ui(5), dupi(5), dui(5)

  type ( nombre_derived_unit_t ), pointer :: duc, duc2, duc5
  character ( len=8 ), dimension ( 64 ) :: str_arr
  character ( len=64 ) :: str

  tester = .describe. "nombre_parse_term"

  call rhyme_nombre_derived_unit_chain_init


  do i = 1, 1
    call random_number( u_rnd )
    call random_number( du_rnd )

    upi = 3 * int( u_rnd(:, 1) * size( prfx_si ) / 3 - 8 + 1 )
    ui = int( u_rnd(:, 2) * size( si_base_units ) + 1 )

    dupi = 3 * int( du_rnd(:, 1) * 16 - 8 + 1 )
    dui = int( du_rnd(:, 2) * size( derived_units ) + 1 )

    ! Skipping kg
    where ( ui .eq. 2 ) ui = 1
    where ( dui .eq. 2 ) dui = 1


    ! base_unit * base_unit
    duc2 => ( prfx_si(upi(1)) * si_base_units(ui(1)) ) &
      * ( prfx_si(upi(2)) * si_base_units(ui(2)) )

    str = add_operator( duc2, [ '*' ] )
    str_arr = rhyme_nombre_parse_tokenize( str )

    duc => rhyme_nombre_parse_term( str_arr, 1 )
    call tester%expect( duc == duc2 .toBe. .true. .hint. str )
    call tester%expect( associated( duc%prev ) .toBe. .false. )
    call tester%expect( associated( duc%next ) .toBe. .false. )


    ! base_unit / base_unit
    duc2 => ( prfx_si(upi(3)) * si_base_units(ui(3)) ) &
      / ( prfx_si(upi(4)) * si_base_units(ui(4)) )

    str = add_operator( duc2, [ '*' ] )
    str_arr = rhyme_nombre_parse_tokenize( str )

    duc => rhyme_nombre_parse_term( str_arr, 1 )
    call tester%expect( duc == duc2 .toBe. .true. .hint. str )
    call tester%expect( associated( duc%prev ) .toBe. .false. )
    call tester%expect( associated( duc%next ) .toBe. .false. )


    ! derived_unit * derived_unit
    duc2 => ( prfx_si(dupi(1)) * derived_units(dui(1)) ) &
      * ( prfx_si(dupi(2)) * derived_units(dui(2)) )

    str = add_operator( duc2, [ '*' ] )
    str_arr = rhyme_nombre_parse_tokenize( str )

    duc => rhyme_nombre_parse_term( str_arr, 1 )
    call tester%expect( duc == duc2 .toBe. .true. .hint. str )
    call tester%expect( duc%next == duc2%next .toBe. .true. )
    call tester%expect( associated( duc%prev ) .toBe. .false. )
    call tester%expect( associated( duc%next%next ) .toBe. .false. )


    ! derived_unit / derived_unit
    duc2 => ( prfx_si(dupi(3)) * derived_units(dui(3)) ) &
      / ( prfx_si(dupi(4)) * derived_units(dui(4)) )

    str = add_operator( duc2, [ '*' ] )
    str_arr = rhyme_nombre_parse_tokenize( str )

    duc => rhyme_nombre_parse_term( str_arr, 1 )
    call tester%expect( duc == duc2 .toBe. .true. .hint. str )
    call tester%expect( duc%next == duc2%next .toBe. .true. )
    call tester%expect( associated( duc%prev ) .toBe. .false. )
    call tester%expect( associated( duc%next%next ) .toBe. .false. )


    ! base_unit * derived_unit
    duc2 => ( prfx_si(upi(1)) * si_base_units(ui(1)) ) &
      * ( prfx_si(dupi(2)) * derived_units(dui(2)) )

    str = add_operator( duc2, [ '*' ] )
    str_arr = rhyme_nombre_parse_tokenize( str )

    duc => rhyme_nombre_parse_term( str_arr, 1 )
    call tester%expect( duc == duc2 .toBe. .true. .hint. str )
    call tester%expect( duc%next == duc2%next .toBe. .true. )
    call tester%expect( associated( duc%prev ) .toBe. .false. )
    call tester%expect( associated( duc%next%next ) .toBe. .false. )


    ! derived_unit * base_unit
    duc2 => ( prfx_si(dupi(1)) * derived_units(dui(1)) ) &
      * ( prfx_si(upi(2)) * si_base_units(ui(2)) )

    str = add_operator( duc2, [ '*' ] )
    str_arr = rhyme_nombre_parse_tokenize( str )

    duc => rhyme_nombre_parse_term( str_arr, 1 )
    call tester%expect( duc == duc2 .toBe. .true. .hint. str )
    call tester%expect( duc%next == duc2%next .toBe. .true. )
    call tester%expect( associated( duc%prev ) .toBe. .false. )
    call tester%expect( associated( duc%next%next ) .toBe. .false. )


    ! derived_unit / base_unit
    duc2 => ( prfx_si(dupi(3)) * derived_units(dui(3)) ) &
      / ( prfx_si(upi(4)) * si_base_units(ui(4)) )

    str = add_operator( duc2, [ '*' ] )
    str_arr = rhyme_nombre_parse_tokenize( str )

    duc => rhyme_nombre_parse_term( str_arr, 1 )
    call tester%expect( duc == duc2 .toBe. .true. .hint. str )
    call tester%expect( duc%next == duc2%next .toBe. .true. )
    call tester%expect( associated( duc%prev ) .toBe. .false. )
    call tester%expect( associated( duc%next%next ) .toBe. .false. )


    ! base_unit / derived_unit
    duc2 => ( prfx_si(upi(3)) * si_base_units(ui(3)) ) &
      / ( prfx_si(dupi(4)) * derived_units(dui(4)) )

    str = add_operator( duc2, [ '*' ] )
    str_arr = rhyme_nombre_parse_tokenize( str )

    duc => rhyme_nombre_parse_term( str_arr, 1 )
    call tester%expect( duc == duc2 .toBe. .true. .hint. str )
    call tester%expect( duc%next == duc2%next .toBe. .true. )
    call tester%expect( associated( duc%prev ) .toBe. .false. )
    call tester%expect( associated( duc%next%next ) .toBe. .false. )


    ! base_unit * derived_unit / base_unit / base_unit * derived_unit
    duc5 => &
      ( prfx_si(upi(1)) * si_base_units(ui(1)) ) &
      / ( prfx_si(dupi(2)) * derived_units(dui(2)) ) &
      / ( prfx_si(upi(3)) * si_base_units(ui(3)) ) &
      / ( prfx_si(upi(4)) * si_base_units(ui(4)) ) &
      * ( prfx_si(dupi(5)) * derived_units(dui(5)) )

    str = add_operator( duc5, [ '*', '*', '*', '*', '*' ] )
    str_arr = rhyme_nombre_parse_tokenize( str )

    duc => rhyme_nombre_parse_term( str_arr, 1 )
    call tester%expect( duc == duc5 .toBe. .true. .hint. str )
    call tester%expect( duc%next == duc5%next .toBe. .true. )
    call tester%expect( duc%next%next == duc5%next%next .toBe. .true. )
    call tester%expect( duc%next%next%next == duc5%next%next%next .toBe. .true. )
    call tester%expect( associated( duc%prev ) .toBe. .false. )
    call tester%expect( associated( duc%next%next%next%next ) .toBe. .false. )
  end do

  failed = tester%failed()
contains

  function add_operator ( duc, op_arr ) result ( output_str )
    implicit none

    type ( nombre_derived_unit_t ) :: duc
    character ( len=* ), dimension (:) :: op_arr
    character ( len=64 ) :: output_str

    character ( len=64 ) :: input_str
    integer :: out_i, in_i, op_i

    input_str = adjustl( .printchain. duc )
    output_str = char(0)

    in_i = 1
    op_i = 1

    do out_i = 1, len_trim( input_str )
      if ( input_str( out_i:out_i ) .eq. ' ' ) then
        output_str( in_i:in_i+1+len_trim(op_arr(op_i))+1 ) = ' '//adjustl(trim( op_arr(op_i) ))//' '
        op_i = op_i + 1
        in_i = in_i + 1+len_trim( op_arr(op_i) )+1
      else
        output_str( in_i:in_i ) = input_str( out_i:out_i )
        in_i = in_i + 1
      end if
    end do
  end function add_operator
end function rhyme_nombre_parse_term_test
