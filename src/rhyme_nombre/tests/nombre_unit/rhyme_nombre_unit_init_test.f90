logical function rhyme_nombre_unit_init_test () result ( failed )
  use rhyme_nombre_unit
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: tester

  tester = .describe. "nombre_unit_init"

  ! call tester%expect( solar_mass%prefix == one .toBe. true .hint. 'solar_mass prefix' )
  ! call tester%expect( solar_mass%conv .toBe. 1.9885d33 .hint. 'solar_mass conv' )
  ! call tester%expect( solar_mass%symb .toBe. 1.9885d33 .hint. 'solar_mass conv' )

  failed = tester%failed()
end function rhyme_nombre_unit_init_test
