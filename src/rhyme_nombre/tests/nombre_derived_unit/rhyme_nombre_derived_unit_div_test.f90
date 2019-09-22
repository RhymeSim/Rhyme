logical function rhyme_nombre_derived_unit_div_test () result ( failed )
  use rhyme_nombre_derived_unit_factory
  use rhyme_nombre_base_unit_assertion
  use rhyme_nombre_dimension_assertion
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: tester

  type ( nombre_unit_t ), pointer :: du
  type ( nombre_base_unit_t ), pointer :: buc
  type ( nombre_base_unit_t ) :: bu(3)
  real ( kind=8 ) :: rnd(3)
  integer :: i

  real ( kind=8 ) :: r8factor
  real ( kind=4 ) :: rfactor
  integer :: ifactor

  tester = .describe. "nombre_derived_unit_div"

  do i = 1, 5
    call random_number( rnd )

    bu = si_base_units( ceiling( rnd * size( si_base_units ) ) )
    buc => nom_du_factory%generate_chain( bu )

    ifactor = int( rnd(1) * 10 - 5 )
    du => ifactor / buc
    call tester%expect( du%conv .toBe. ifactor )
    call tester%expect( du%head .toBe. bu(1)**(-1) )
    call tester%expect( du%head%next .toBe. bu(2)**(-1) )
    call tester%expect( du%head%next%next .toBe. bu(3)**(-1) )
    call tester%expect( du%dim .toBe. rhyme_nombre_base_unit_chain_get_dim( buc )**(-1) )

    rfactor = real( rnd(2) * 10 - 5 )
    du => rfactor / buc
    call tester%expect( du%conv .toBe. rfactor )
    call tester%expect( du%head .toBe. bu(1)**(-1) )
    call tester%expect( du%head%next .toBe. bu(2)**(-1) )
    call tester%expect( du%head%next%next .toBe. bu(3)**(-1) )
    call tester%expect( du%dim .toBe. rhyme_nombre_base_unit_chain_get_dim( buc )**(-1) )

    r8factor = rnd(2) * 10 - 5
    du => r8factor / buc
    call tester%expect( du%conv .toBe. r8factor )
    call tester%expect( du%head .toBe. bu(1)**(-1) )
    call tester%expect( du%head%next .toBe. bu(2)**(-1) )
    call tester%expect( du%head%next%next .toBe. bu(3)**(-1) )
    call tester%expect( du%dim .toBe. rhyme_nombre_base_unit_chain_get_dim( buc )**(-1) )
  end do

  failed = tester%failed()
end function rhyme_nombre_derived_unit_div_test
