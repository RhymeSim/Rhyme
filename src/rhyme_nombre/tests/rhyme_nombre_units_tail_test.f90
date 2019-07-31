logical function rhyme_nombre_units_tail_test () result ( failed )
  use rhyme_nombre_units
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: n_tester
  type ( nombre_unit_t ), pointer :: u, tail

  n_tester = .describe. "rhyme_nombre_units_tail"

  u => meter**2 * ( kilo * gram ) / sec**2 / kel

  tail => rhyme_nombre_units_tail( u )
  call n_tester%expect( tail%symb .toBe. 'K' )

  failed = n_tester%failed()
end function rhyme_nombre_units_tail_test
