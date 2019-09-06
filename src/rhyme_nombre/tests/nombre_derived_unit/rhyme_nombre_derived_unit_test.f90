logical function rhyme_nombre_derived_unit_test () result ( failed )
  use rhyme_nombre_derived_unit
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: tester

  tester = .describe. "nombre_derived_unit"

  failed = tester%failed()
end function rhyme_nombre_derived_unit_test
