logical function rhyme_nombre_base_unit_print_test () result ( failed )
  use rhyme_nombre_base_unit
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: tester

  type ( nombre_base_unit_t ), pointer :: u
  character ( len=64 ) :: str

  tester = .describe. "nombre_base_unit_print"

  str = .print. gram
  call tester%expect( str .toBe. 'g' )

  str = .print. kilogram
  call tester%expect( str .toBe. 'kg' )

  str = .print. meter
  call tester%expect( str .toBe. 'm' )

  str = .print. second
  call tester%expect( str .toBe. 's' )

  str = .print. kelvin
  call tester%expect( str .toBe. 'K' )

  str = .print. ampere
  call tester%expect( str .toBe. 'A' )

  str = .print. mole
  call tester%expect( str .toBe. 'mol' )

  str = .print. candela
  call tester%expect( str .toBe. 'cd' )

  u => kilogram**2
  str = .print. u
  call tester%expect( str .toBe. 'kg^2' )

  u => giga * second
  str = .print. u
  call tester%expect( str .toBe. 'Gs' )

  u => nano * kelvin**(-4)
  str = .print. u
  call tester%expect( str .toBe. 'nK^-4' )

  failed = tester%failed()
end function rhyme_nombre_base_unit_print_test
