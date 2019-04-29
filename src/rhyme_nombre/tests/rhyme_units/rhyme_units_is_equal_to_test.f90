logical function rhyme_units_is_equal_to_test () result (failed)
  use rhyme_units
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: n_tester

  type(unit_t), pointer :: u1, u2

  n_tester = .describe. "nombre_units_is_equal_to"

  u1 => kg * (meter / sec)**2
  u2 => kg

  call n_tester%expect( u1 .unitEqualsTo. u2 .toBe. .false. )

  u2 => u2 * (meter / sec)**2

  call n_tester%expect( u1 .unitEqualsTo. u2 .toBe. .true. )

  failed = n_tester%failed()
end function rhyme_units_is_equal_to_test
