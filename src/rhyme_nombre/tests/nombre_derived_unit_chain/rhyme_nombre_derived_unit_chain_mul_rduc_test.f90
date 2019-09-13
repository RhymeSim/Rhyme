logical function rhyme_nombre_derived_unit_chain_mul_rduc_test () result ( failed )
  use rhyme_nombre_derived_unit_chain_factory
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: tester

  type ( nombre_derived_unit_t ), pointer :: duc
  type ( nombre_derived_unit_t ) :: du(3)
  real ( kind=8 ) :: rnd(3)
  integer :: idx(3), i

  tester = .describe. "nombre_derived_unit_chain_mul_rduc"

  do i = 1, 1
    call random_number( rnd )
    idx = rnd * size( derived_units ) + 1
    du = derived_units( idx )

    rnd = rnd * 10 - 5

    duc => real( rnd(1), kind=4 ) * du(1)
    call tester%expect( associated( duc%prev ) .toBe. .false. .hint. 'left end' )
    call tester%expect( duc%conv == real( rnd(1) , kind=4) * du(1)%conv .toBe. .true. )
    call tester%expect( associated( duc%next%next ) .toBe. .false. .hint. 'right end' )

    duc => (real( rnd(1), kind=4 ) * du(1)) * (real( rnd(2), kind=4 ) * du(2))
    call tester%expect( associated( duc%prev ) .toBe. .false. .hint. 'left end' )
    call tester%expect( duc%conv == real( rnd(1), kind=4 ) * du(1)%conv .toBe. .true. )
    call tester%expect( duc%next%conv == real( rnd(2), kind=4 ) * du(2)%conv .toBe. .true. )
    call tester%expect( associated( duc%next%next%next ) .toBe. .false. .hint. 'right end' )

    duc => real( rnd(1), kind=4 ) * ( du(1) * du(2) )
    call tester%expect( associated( duc%prev ) .toBe. .false. .hint. 'left end' )
    call tester%expect( duc%conv == real( rnd(1), kind=4 ) * du(1)%conv .toBe. .true. )
    call tester%expect( duc%next%conv == real( rnd(1), kind=4 ) * du(2)%conv .toBe. .true. )
    call tester%expect( associated( duc%next%next%next ) .toBe. .false. .hint. 'right end' )

    duc => (real( rnd(1), kind=4 ) * du(1)) * (real( rnd(2), kind=4 ) * du(2)) * (real( rnd(3), kind=4 ) * du(3))
    call tester%expect( associated( duc%prev ) .toBe. .false. .hint. 'left end' )
    call tester%expect( duc%conv == real( rnd(1), kind=4 ) * du(1)%conv .toBe. .true. )
    call tester%expect( duc%next%conv == real( rnd(2), kind=4 ) * du(2)%conv .toBe. .true. )
    call tester%expect( duc%next%next%conv == real( rnd(3), kind=4 ) * du(3)%conv .toBe. .true. )
    call tester%expect( associated( duc%next%next%next%next ) .toBe. .false. .hint. 'right end' )

    duc => real( rnd(1), kind=4 ) * ( du(1) * du(2) * du(3) )
    call tester%expect( associated( duc%prev ) .toBe. .false. .hint. 'left end' )
    call tester%expect( duc%conv == real( rnd(1), kind=4 ) * du(1)%conv .toBe. .true. )
    call tester%expect( duc%next%conv == real( rnd(1), kind=4 ) * du(2)%conv .toBe. .true. )
    call tester%expect( duc%next%next%conv == real( rnd(1), kind=4 ) * du(3)%conv .toBe. .true. )
    call tester%expect( associated( duc%next%next%next%next ) .toBe. .false. .hint. 'right end' )
  end do

  failed = tester%failed()
end function rhyme_nombre_derived_unit_chain_mul_rduc_test
