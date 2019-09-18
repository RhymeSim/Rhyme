logical function rhyme_nombre_derived_unit_chain_mul_pduc_test () result ( failed )
  use rhyme_nombre_derived_unit_chain_factory
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: tester

  type ( nombre_derived_unit_t ), pointer :: duc
  type ( nombre_derived_unit_t ) :: du(3)
  type ( nombre_prefix_t ) :: prfx(3)
  real ( kind=8 ) :: rnd(3)
  integer :: idx(3), p_idx(3), i

  tester = .describe. "nombre_derived_unit_chain_mul_pduc"

  do i = 1, 1
    call random_number( rnd )

    idx = int( rnd * size( derived_units ) + 1 )
    p_idx = int( rnd * size( prfx_si ) + 1 )

    du = derived_units( idx )
    prfx = prfx_si( p_idx )

    duc => prfx(1) * du(1)
    call tester%expect( associated( duc%prev ) .toBe. .false. .hint. 'left end' )
    call tester%expect( duc%prefix == prfx(1) * du(1)%prefix .toBe. .true. )
    call tester%expect( associated( duc%next%next ) .toBe. .false. .hint. 'right end' )

    duc => (prfx(1) * du(1)) * (prfx(2) * du(2))
    call tester%expect( associated( duc%prev ) .toBe. .false. .hint. 'left end' )
    call tester%expect( duc%prefix == prfx(1) * du(1)%prefix .toBe. .true. )
    call tester%expect( duc%next%prefix == prfx(2) * du(2)%prefix .toBe. .true. )
    call tester%expect( associated( duc%next%next%next ) .toBe. .false. .hint. 'right end' )

    duc => prfx(1) * ( du(1) * du(2) )
    call tester%expect( associated( duc%prev ) .toBe. .false. .hint. 'left end' )
    call tester%expect( duc%prefix == prfx(1) * du(1)%prefix .toBe. .true. )
    call tester%expect( duc%next%prefix == prfx(1) * du(2)%prefix .toBe. .true. )
    call tester%expect( associated( duc%next%next%next ) .toBe. .false. .hint. 'right end' )

    duc => (prfx(1) * du(1)) * (prfx(2) * du(2)) * (prfx(3) * du(3))
    call tester%expect( associated( duc%prev ) .toBe. .false. .hint. 'left end' )
    call tester%expect( duc%prefix == prfx(1) * du(1)%prefix .toBe. .true. )
    call tester%expect( duc%next%prefix == prfx(2) * du(2)%prefix .toBe. .true. )
    call tester%expect( duc%next%next%prefix == prfx(3) * du(3)%prefix .toBe. .true. )
    call tester%expect( associated( duc%next%next%next%next ) .toBe. .false. .hint. 'right end' )

    duc => prfx(1) * ( du(1) * du(2) * du(3) )
    call tester%expect( associated( duc%prev ) .toBe. .false. .hint. 'left end' )
    call tester%expect( duc%prefix == prfx(1) * du(1)%prefix .toBe. .true. )
    call tester%expect( duc%next%prefix == prfx(1) * du(2)%prefix .toBe. .true. )
    call tester%expect( duc%next%next%prefix == prfx(1) * du(3)%prefix .toBe. .true. )
    call tester%expect( associated( duc%next%next%next%next ) .toBe. .false. .hint. 'right end' )
  end do

  failed = tester%failed()
end function rhyme_nombre_derived_unit_chain_mul_pduc_test
