logical function rhyme_nombre_base_unit_chain_print_test () result ( failed )
  use rhyme_nombre_base_unit_chain_factory
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: tester

  character ( len=64 ) :: str
  type ( nombre_base_unit_t ), pointer :: u

  tester = .describe. "nombre_base_unit_chain"

  u => nom_buc_factory%generate( [ kilogram, meter, second**(-2) ] )
  str = .printchain. u
  call tester%expect( str .toBe. 'kg m s^-2' )

  u => nom_buc_factory%generate( [ kelvin, second**(-.5d0) ] )
  str = .printchain. u
  call tester%expect( str .toBe. 'K s^-.50' )

  failed = tester%failed()
end function rhyme_nombre_base_unit_chain_print_test
