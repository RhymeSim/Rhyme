logical function rhyme_nombre_derived_unit_print_test () result ( failed )
  use rhyme_nombre_derived_unit_factory
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: tester

  type ( nombre_derived_unit_t ), pointer :: du

  tester = .describe. "nombre_derived_unit_print"

  call rhyme_nombre_derived_unit_init

  call tester%expect( .print. solar_mass .toBe. 'Msun' )

  call tester%expect( .print. hydrogen_mass .toBe. 'm_H' )

  du => kilo * solar_mass**2
  call tester%expect( .print. du .toBe. 'kMsun^2' )

  du => nano * solar_mass**5d-1
  call tester%expect( .print. du .toBe. 'nMsun^.50' )

  du%head => nom_du_factory%generate_chain( second**(-1), kilogram, (kilo * meter)**(-2.5) )
  du%symb = ''
  call tester%expect( .print. du .toBe. 's^-1 kg km^-2.50' )

  failed = tester%failed()
end function rhyme_nombre_derived_unit_print_test
