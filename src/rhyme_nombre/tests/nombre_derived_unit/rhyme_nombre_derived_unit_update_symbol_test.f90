logical function rhyme_nombre_derived_unit_update_symbol_test () result ( failed )
  use rhyme_nombre_derived_unit
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: tester

  type ( nombre_derived_unit_t ), pointer :: chain

  tester = .describe. "nombre_derived_unit_update_symbol"

  chain => kilogram * meter**2 / second**2 .as. 'J'

  call tester%expect( chain%symb .toBe. 'J' .hint. 'symbol' )
  call tester%expect( chain%head == kilogram .toBe. .true. .hint. 'kg' )
  call tester%expect( chain%head%next == meter**2 .toBe. .true. .hint. 'meter' )
  call tester%expect( chain%head%next%next == second**(-2) .toBe. .true. .hint. 'sec' )

  failed = tester%failed()
end function rhyme_nombre_derived_unit_update_symbol_test
