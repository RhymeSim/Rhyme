logical function rhyme_nombre_parse_find_unit_test () result ( failed )
  use rhyme_nombre_parse
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: tester
  type ( nombre_base_unit_t ), pointer :: unit, new_unit

  integer :: u, p
  character ( len=128 ) :: msg


  tester = .describe. "nombre_parse_find_unit"

  unit => rhyme_nombre_parse_find_unit( 'g' )
  call tester%expect( unit == gram .toBe. .true. .hint. 'g' )

  unit => rhyme_nombre_parse_find_unit( 'kg' )
  call tester%expect( unit == kilogram .toBe. .true. .hint. 'kg' )

  unit => rhyme_nombre_parse_find_unit( 'm' )
  call tester%expect( unit == meter .toBe. .true. .hint. 'm' )

  unit => rhyme_nombre_parse_find_unit( 's' )
  call tester%expect( unit == second .toBe. .true. .hint. 's' )

  unit => rhyme_nombre_parse_find_unit( 'K' )
  call tester%expect( unit == kelvin .toBe. .true. .hint. 'K' )

  unit => rhyme_nombre_parse_find_unit( 'A' )
  call tester%expect( unit == ampere .toBe. .true. .hint. 'A' )

  unit => rhyme_nombre_parse_find_unit( 'mol' )
  call tester%expect( unit == mole .toBe. .true. .hint. 'mol' )

  unit => rhyme_nombre_parse_find_unit( 'cd' )
  call tester%expect( unit == candela .toBe. .true.  .hint. 'cd')

  unit => rhyme_nombre_parse_find_unit( 'none' )
  call tester%expect( associated( unit ) .toBe. .false. .hint. 'none' )

  do p = -24, 24
    if ( len_trim( prfx_si(p)%symb ) .eq. 0 ) cycle

    do u = 1, size( si_base_units )
      if ( si_base_units(u) == kilogram ) cycle

      new_unit => prfx_si(p) * si_base_units(u)
      unit => rhyme_nombre_parse_find_unit( .print. new_unit )

      write ( msg, * ) trim((.print. unit)), ' == ', trim((.print. new_unit))

      call tester%expect( unit == new_unit .toBe. .true. .hint. msg )
    end do
  end do

  failed = tester%failed()
end function rhyme_nombre_parse_find_unit_test
