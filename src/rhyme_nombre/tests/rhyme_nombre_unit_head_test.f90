logical function rhyme_nombre_unit_head_test () result ( failed )
  use rhyme_nombre_units
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: n_tester
  type ( nombre_unit_t ), pointer :: u, head

  u => ( kilo * gram ) * meter / sec**2

  n_tester = .describe. "rhyme_nombre_unit_head"

  head => rhyme_nombre_unit_head( u )
  call n_tester%expect( head%symb .toBe. 'g' )

  failed = n_tester%failed()
end function rhyme_nombre_unit_head_test
