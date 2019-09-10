logical function rhyme_nombre_derived_unit_print_test () result ( failed )
  use rhyme_nombre_derived_unit_factory
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: tester

  type ( nombre_derived_unit_t ), pointer :: du, kdu2, ndu5

  tester = .describe. "nombre_derived_unit_print"

  du => nom_du_factory%generate( [ kilogram, meter**2, second**(-2) ], 'J' )
  call tester%expect( .print. du .toBe. 'J' )

  kdu2 => kilo * du
  kdu2%pow = 2
  call tester%expect( .print. kdu2 .toBe. 'kJ^2' )

  ndu5 => nano * du
  ndu5%pow = 5d-1
  call tester%expect( .print. ndu5 .toBe. 'nJ^.50' )

  du%symb = ''
  call tester%expect( .print. du .toBe. 'kg m^2 s^-2' )

  failed = tester%failed()
end function rhyme_nombre_derived_unit_print_test
