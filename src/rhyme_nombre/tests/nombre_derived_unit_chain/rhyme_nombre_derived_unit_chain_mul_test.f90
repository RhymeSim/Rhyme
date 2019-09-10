logical function rhyme_nombre_derived_unit_chain_mul_test () result ( failed )
  use rhyme_nombre_derived_unit_chain_factory
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: tester

  type ( nombre_derived_unit_t ), pointer :: du, duu, duuu
  type ( nombre_derived_unit_t ), pointer :: udu, uudu

  tester = .describe. "nombre_derived_unit_chain_mul"

  call rhyme_nombre_derived_unit_chain_init

  du => atomic_mass_unit * astronomical_unit * stradian

  call tester%expect( associated( du%prev ) .toBe. .false. )
  call tester%expect( du == atomic_mass_unit .toBe. .true. )
  call tester%expect( du%next == astronomical_unit .toBe. .true. )
  call tester%expect( du%next%prev == atomic_mass_unit .toBe. .true. )
  call tester%expect( du%next%next == stradian .toBe. .true. )
  call tester%expect( du%next%next%prev == astronomical_unit .toBe. .true. )
  call tester%expect( associated( du%next%next%next ) .toBe. .false. )

  duu => du * second
  call tester%expect( duu == atomic_mass_unit .toBe. .true. )
  call tester%expect( duu%next == astronomical_unit .toBe. .true. )
  call tester%expect( duu%next%next == stradian .toBe. .true. )
  call tester%expect( duu%next%next%next%head == second .toBe. .true. )

  duuu => duu * kilogram**2
  call tester%expect( duuu == atomic_mass_unit .toBe. .true. )
  call tester%expect( duuu%next == astronomical_unit .toBe. .true. )
  call tester%expect( duuu%next%next == stradian .toBe. .true. )
  call tester%expect( duuu%next%next%next%head == second .toBe. .true. )
  call tester%expect( duuu%next%next%next%head%next == kilogram**2 .toBe. .true. )

  udu => kelvin * du
  call tester%expect( udu%head == kelvin .toBe. .true. )
  call tester%expect( udu%next == atomic_mass_unit .toBe. .true. )
  call tester%expect( udu%next%next == astronomical_unit .toBe. .true. )
  call tester%expect( udu%next%next%next == stradian .toBe. .true. )
  call tester%expect( associated( udu%next%next%next%next ) .toBe. .false. )

  uudu => meter**2 * udu
  call tester%expect( uudu%head == meter**2 .toBe. .true. )
  call tester%expect( uudu%head%next == kelvin .toBe. .true. )
  call tester%expect( uudu%next == atomic_mass_unit .toBe. .true. )
  call tester%expect( uudu%next%next == astronomical_unit .toBe. .true. )
  call tester%expect( uudu%next%next%next == stradian .toBe. .true. )
  call tester%expect( associated( udu%next%next%next%next ) .toBe. .false. )

  failed = tester%failed()
end function rhyme_nombre_derived_unit_chain_mul_test
