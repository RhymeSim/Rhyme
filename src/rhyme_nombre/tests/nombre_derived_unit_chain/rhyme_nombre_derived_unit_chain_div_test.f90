logical function rhyme_nombre_derived_unit_chain_div_test () result ( failed )
  use rhyme_nombre_derived_unit_chain_factory
  use rhyme_nombre_derived_unit_factory
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: tester

  type ( nombre_derived_unit_t ), pointer :: du, duu
  type ( nombre_derived_unit_t ), pointer :: duc, ducu

  tester = .describe. "nombre_derived_unit_chain_div"

  du => nom_du_factory%generate( [ kilogram, meter, second**(-2) ], 'N' )
  duu => du / meter**2
  call tester%expect( duu == du .toBe. .true. )
  call tester%expect( duu%next%head == meter**(-2) .toBe. .true. )
  call tester%expect( associated( duu%next%next ) .toBe. .false. )

  du%symb = ''
  duu => du / second
  call tester%expect( duu == du .toBe. .false. )
  call tester%expect( duu%head == kilogram .toBe. .true. )
  call tester%expect( duu%head%next == meter .toBe. .true. )
  call tester%expect( duu%head%next%next == second**(-2) .toBe. .true. )
  call tester%expect( duu%head%next%next%next == second**(-1) .toBe. .true. )
  call tester%expect( associated( duu%next ) .toBe. .false. )


  ! duc => nom_duc_factory%generate

  failed = tester%failed()
end function rhyme_nombre_derived_unit_chain_div_test
