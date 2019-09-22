logical function rhyme_nombre_unit_mul_iduc_test () result ( failed )
  use rhyme_nombre_unit_factory
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: tester

  type ( nombre_unit_t ), pointer :: duc
  type ( nombre_unit_t ) :: du(3)
  real ( kind=8 ) :: rnd(3)
  integer ::  i, ifactor(3)

  tester = .describe. "nombre_unit_mul_iduc"

  call rhyme_nombre_derived_unit_init

  do i = 1, 5
    call random_number( rnd )
    du = derived_units( ceiling( rnd * size( derived_units ) ) )

    rnd = rnd * 1000 - 500
    ifactor = int( rnd * 10 - 5 )

    duc => ifactor(1) * du(1)
    call tester%expect( associated( duc%prev ) .toBe. .false. .hint. 'left end' )
    call tester%expect( duc%conv .toBe. ifactor(1) * du(1)%conv )
    call tester%expect( associated( duc%next ) .toBe. .false. .hint. 'right end' )

    duc => (ifactor(1) * du(1)) * (ifactor(2) * du(2))
    call tester%expect( associated( duc%prev ) .toBe. .false. .hint. 'left end' )
    call tester%expect( duc%conv .toBe. ifactor(1) * du(1)%conv )
    call tester%expect( duc%next%conv .toBe. ifactor(2) * du(2)%conv )
    call tester%expect( associated( duc%next%next ) .toBe. .false. .hint. 'right end' )

    duc => ifactor(1) * ( du(1) * du(2) )
    call tester%expect( associated( duc%prev ) .toBe. .false. .hint. 'left end' )
    call tester%expect( duc%conv .toBe. ifactor(1) * du(1)%conv )
    call tester%expect( duc%next%conv .toBe. ifactor(1) * du(2)%conv )
    call tester%expect( associated( duc%next%next ) .toBe. .false. .hint. 'right end' )

    duc => (ifactor(1) * du(1)) * (ifactor(2) * du(2)) * (ifactor(3) * du(3))
    call tester%expect( associated( duc%prev ) .toBe. .false. .hint. 'left end' )
    call tester%expect( duc%conv .toBe. ifactor(1) * du(1)%conv )
    call tester%expect( duc%next%conv .toBe. ifactor(2) * du(2)%conv )
    call tester%expect( duc%next%next%conv .toBe. ifactor(3) * du(3)%conv )
    call tester%expect( associated( duc%next%next%next ) .toBe. .false. .hint. 'right end' )

    duc => ifactor(1) * ( du(1) * du(2) * du(3) )
    call tester%expect( associated( duc%prev ) .toBe. .false. .hint. 'left end' )
    call tester%expect( duc%conv .toBe. ifactor(1) * du(1)%conv )
    call tester%expect( duc%next%conv .toBe. ifactor(1) * du(2)%conv )
    call tester%expect( duc%next%next%conv .toBe. ifactor(1) * du(3)%conv )
    call tester%expect( associated( duc%next%next%next ) .toBe. .false. .hint. 'right end' )
  end do

  failed = tester%failed()
end function rhyme_nombre_unit_mul_iduc_test
