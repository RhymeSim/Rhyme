logical function rhyme_nombre_derived_unit_get_dim_test () result ( failed )
  use rhyme_nombre_derived_unit
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: tester

  type ( nombre_derived_unit_t ), pointer :: chain
  type ( nombre_dimension_t ) :: dim, expected_dim

  tester = .describe. "nombre_derived_unit_get_dim"

  chain => kilogram * meter**2 / second**2
  dim = rhyme_nombre_derived_unit_get_dim( chain )
  expected_dim%powers = dimid%mass%powers + 2 * dimid%length%powers - 2 * dimid%time%powers
  expected_dim%symb = 'M L^2 T^-2'

  call tester%expect( dim%powers .toBe. expected_dim%powers )
  call tester%expect( dim%symb .toBe. expected_dim%symb )

  chain => kelvin / second * meter**5d-1
  dim = rhyme_nombre_derived_unit_get_dim( chain )
  expected_dim%powers = dimid%theta%powers - dimid%time%powers + 5d-1 * dimid%length%powers
  expected_dim%symb = 'Theta T^-1 L^.50'

  call tester%expect( dim%powers .toBe. expected_dim%powers )
  call tester%expect( dim%symb .toBe. expected_dim%symb )

  failed = tester%failed()
end function rhyme_nombre_derived_unit_get_dim_test
