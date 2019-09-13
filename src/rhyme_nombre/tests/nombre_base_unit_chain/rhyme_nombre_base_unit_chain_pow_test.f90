logical function rhyme_nombre_base_unit_chain_pow_test () result ( failed )
  use rhyme_nombre_base_unit_chain_factory
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: tester
  type ( nombre_base_unit_t ), pointer :: u

  tester = .describe. "nombre_base_unit_chain_pow"

  u => kilogram**2
  call tester%expect( u%pow .toBe. real( 2, kind=8 ) )

  u => meter**1.23e0
  call tester%expect( u%pow .toBe. 1.23e0 )

  u => kelvin**2.34d0
  call tester%expect( u%pow .toBe. 2.34d0 )

  u => nom_buc_factory%generate( [ kilogram, meter, second**(-2) ] )
  u => u**(-1)
  call tester%expect( u == kilogram**(-1) .toBe. .true. )
  call tester%expect( u%next == meter**(-1) .toBe. .true. )
  call tester%expect( u%next%next == second**2 .toBe. .true. )
  call tester%expect( associated( u%prev ) .toBe. .false. )
  call tester%expect( associated( u%next%next%next ) .toBe. .false. )

  u => nom_buc_factory%generate( [ meter**2, meter**(-2) ] )
  u => u**1.23e4
  call tester%expect( u == meter**(1.23e4 * 2) .toBe. .true. )
  call tester%expect( u%next == meter**(1.23e4 * (-2)) .toBe. .true. )
  call tester%expect( associated( u%prev ) .toBe. .false. )
  call tester%expect( associated( u%next%next ) .toBe. .false. )

  u => nom_buc_factory%generate( [ kilogram, meter, second**(-2) ] )
  u => u**(-2.34d1)
  call tester%expect( u == kilogram**(-2.34d1) .toBe. .true. )
  call tester%expect( u%next == meter**(-2.34d1) .toBe. .true. )
  call tester%expect( u%next%next == second**(-2.34d1 * (-2)) .toBe. .true. )
  call tester%expect( associated( u%prev ) .toBe. .false. )
  call tester%expect( associated( u%next%next%next ) .toBe. .false. )

  failed = tester%failed()
end function rhyme_nombre_base_unit_chain_pow_test
