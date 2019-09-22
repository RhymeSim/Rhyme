logical function rhyme_nombre_unit_mul_pduc_test () result ( failed )
  use rhyme_nombre_unit_factory
  use rhyme_nombre_prefix_assertion
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: tester

  type ( nombre_unit_t ), pointer :: duc
  type ( nombre_unit_t ) :: du(3)
  type ( nombre_prefix_t ) :: prfx(3)
  real ( kind=8 ) :: rnd(3)
  integer :: i

  tester = .describe. "nombre_unit_mul_pduc"

  call rhyme_nombre_derived_unit_init

  do i = 1, 1
    call random_number( rnd )

    du = derived_units( ceiling( rnd * size( derived_units ) ) )
    prfx = prfx_si( ceiling( rnd * size( prfx_si ) ) )

    duc => prfx(1) * du(1)
    call tester%expect( associated( duc%prev ) .toBe. .false. .hint. 'left end' )
    call tester%expect( duc%prefix .toBe. prfx(1) * du(1)%prefix )
    call tester%expect( associated( duc%next ) .toBe. .false. .hint. 'right end' )

    duc => (prfx(1) * du(1)) * (prfx(2) * du(2))
    call tester%expect( associated( duc%prev ) .toBe. .false. .hint. 'left end' )
    call tester%expect( duc%prefix .toBe. prfx(1) * du(1)%prefix )
    call tester%expect( duc%next%prefix .toBe. prfx(2) * du(2)%prefix )
    call tester%expect( associated( duc%next%next ) .toBe. .false. .hint. 'right end' )

    duc => prfx(1) * ( du(1) * du(2) )
    call tester%expect( associated( duc%prev ) .toBe. .false. .hint. 'left end' )
    call tester%expect( duc%prefix .toBe. prfx(1) * du(1)%prefix )
    call tester%expect( duc%next%prefix .toBe. prfx(1) * du(2)%prefix )
    call tester%expect( associated( duc%next%next ) .toBe. .false. .hint. 'right end' )

    duc => (prfx(1) * du(1)) * (prfx(2) * du(2)) * (prfx(3) * du(3))
    call tester%expect( associated( duc%prev ) .toBe. .false. .hint. 'left end' )
    call tester%expect( duc%prefix .toBe. prfx(1) * du(1)%prefix )
    call tester%expect( duc%next%prefix .toBe. prfx(2) * du(2)%prefix )
    call tester%expect( duc%next%next%prefix .toBe. prfx(3) * du(3)%prefix )
    call tester%expect( associated( duc%next%next%next ) .toBe. .false. .hint. 'right end' )

    duc => prfx(1) * ( du(1) * du(2) * du(3) )
    call tester%expect( associated( duc%prev ) .toBe. .false. .hint. 'left end' )
    call tester%expect( duc%prefix .toBe. prfx(1) * du(1)%prefix )
    call tester%expect( duc%next%prefix .toBe. prfx(1) * du(2)%prefix )
    call tester%expect( duc%next%next%prefix .toBe. prfx(1) * du(3)%prefix )
    call tester%expect( associated( duc%next%next%next ) .toBe. .false. .hint. 'right end' )
  end do

  failed = tester%failed()
end function rhyme_nombre_unit_mul_pduc_test
