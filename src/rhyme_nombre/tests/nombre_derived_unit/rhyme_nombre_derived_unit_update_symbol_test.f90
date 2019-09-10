logical function rhyme_nombre_derived_unit_update_symbol_test () result ( failed )
  use rhyme_nombre_derived_unit_factory
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: tester

  type ( nombre_derived_unit_t ), pointer :: dunit

  tester = .describe. "nombre_derived_unit_update_symbol"

  dunit => nom_du_factory%generate( [ kilogram, meter**2, second**(-2) ], '' )
  dunit => dunit .as. 'J'

  call tester%expect( dunit%symb .toBe. 'J' .hint. 'symbol' )
  call tester%expect( dunit%head == kilogram .toBe. .true. .hint. 'kg' )
  call tester%expect( dunit%head%next == meter**2 .toBe. .true. .hint. 'meter' )
  call tester%expect( dunit%head%next%next == second**(-2) .toBe. .true. .hint. 'sec' )

  failed = tester%failed()
end function rhyme_nombre_derived_unit_update_symbol_test
