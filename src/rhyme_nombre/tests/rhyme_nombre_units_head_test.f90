logical function rhyme_nombre_units_head_test () result ( failed )
  use rhyme_nombre_units
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: n_tester
  type ( nombre_unit_t ), pointer :: u, head

  n_tester = .describe. "rhyme_nombre_units_head"

  call rhyme_nombre_units_init

  u => ( kilo * gram ) * meter / sec**2

  head => rhyme_nombre_units_head( u )
  call n_tester%expect( head%symb .toBe. 'g' )

  failed = n_tester%failed()
end function rhyme_nombre_units_head_test
