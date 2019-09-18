logical function rhyme_nombre_derived_unit_chain_mul_iduc_test () result ( failed )
  use rhyme_nombre_derived_unit_chain_factory
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: tester

  type ( nombre_derived_unit_t ), pointer :: duc
  type ( nombre_derived_unit_t ) :: du(3)
  real ( kind=8 ) :: rnd(3)
  integer :: idx(3), i

  tester = .describe. "nombre_derived_unit_chain_mul_iduc"

  do i = 1, 1
    call random_number( rnd )
    idx = int( rnd * size( derived_units ) + 1 )
    du = derived_units( idx )

    rnd = rnd * 1000 - 500

    duc => int( rnd(1) ) * du(1)
    call tester%expect( associated( duc%prev ) .toBe. .false. .hint. 'left end' )
    call tester%expect( duc%conv .toBe. int( rnd(1) ) * du(1)%conv )
    call tester%expect( associated( duc%next%next ) .toBe. .false. .hint. 'right end' )

    duc => (int( rnd(1) ) * du(1)) * (int( rnd(2) ) * du(2))
    call tester%expect( associated( duc%prev ) .toBe. .false. .hint. 'left end' )
    call tester%expect( duc%conv .toBe. int( rnd(1) ) * du(1)%conv )
    call tester%expect( duc%next%conv .toBe. int( rnd(2) ) * du(2)%conv )
    call tester%expect( associated( duc%next%next%next ) .toBe. .false. .hint. 'right end' )

    duc => int( rnd(1) ) * ( du(1) * du(2) )
    call tester%expect( associated( duc%prev ) .toBe. .false. .hint. 'left end' )
    call tester%expect( duc%conv .toBe. int( rnd(1) ) * du(1)%conv )
    call tester%expect( duc%next%conv .toBe. int( rnd(1) ) * du(2)%conv )
    call tester%expect( associated( duc%next%next%next ) .toBe. .false. .hint. 'right end' )

    duc => (int( rnd(1) ) * du(1)) * (int( rnd(2) ) * du(2)) * (int( rnd(3) ) * du(3))
    call tester%expect( associated( duc%prev ) .toBe. .false. .hint. 'left end' )
    call tester%expect( duc%conv .toBe. int( rnd(1) ) * du(1)%conv )
    call tester%expect( duc%next%conv .toBe. int( rnd(2) ) * du(2)%conv )
    call tester%expect( duc%next%next%conv .toBe. int( rnd(3) ) * du(3)%conv )
    call tester%expect( associated( duc%next%next%next%next ) .toBe. .false. .hint. 'right end' )

    duc => int( rnd(1) ) * ( du(1) * du(2) * du(3) )
    call tester%expect( associated( duc%prev ) .toBe. .false. .hint. 'left end' )
    call tester%expect( duc%conv .toBe. int( rnd(1) ) * du(1)%conv )
    call tester%expect( duc%next%conv .toBe. int( rnd(1) ) * du(2)%conv )
    call tester%expect( duc%next%next%conv .toBe. int( rnd(1) ) * du(3)%conv )
    call tester%expect( associated( duc%next%next%next%next ) .toBe. .false. .hint. 'right end' )
  end do

  failed = tester%failed()
end function rhyme_nombre_derived_unit_chain_mul_iduc_test
