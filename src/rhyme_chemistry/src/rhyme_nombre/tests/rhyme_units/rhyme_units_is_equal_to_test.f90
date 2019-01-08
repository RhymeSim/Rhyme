logical function rhyme_units_is_equal_to_test () result (failed)
  use rhyme_units

  implicit none

  type(unit_t), pointer :: u1, u2

  u1 => kg * (m / s)**2
  u2 => kg

  failed = .not. (u1 .unitEqualsTo. u2) .eqv. .false.

  if ( failed ) return

  u2 => u2 * (m / s)**2

  failed = .not. (u1 .unitEqualsTo. u2) .eqv. .true.
end function rhyme_units_is_equal_to_test
