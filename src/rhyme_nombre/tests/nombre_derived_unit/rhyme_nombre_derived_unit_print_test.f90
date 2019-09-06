logical function rhyme_nombre_derived_unit_print_test () result ( failed )
  use rhyme_nombre_derived_unit
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: tester

  type ( nombre_derived_unit_t ), pointer :: du
  character ( len=64 ) :: str

  tester = .describe. "nombre_derived_unit_print"

  call rhyme_nombre_derived_unit_init

  str = .print. solar_mass
  call tester%expect( str .toBe. 'Msun' )

  str = .print. hydrogen_mass
  call tester%expect( str .toBe. 'm_H' )

  du => kilo * solar_mass**2
  str = .print. du
  call tester%expect( str .toBe. 'kMsun^2' )

  du => nano * solar_mass**5d-1
  str = .print. du
  call tester%expect( str .toBe. 'nMsun^.50' )

  failed = tester%failed()
end function rhyme_nombre_derived_unit_print_test
