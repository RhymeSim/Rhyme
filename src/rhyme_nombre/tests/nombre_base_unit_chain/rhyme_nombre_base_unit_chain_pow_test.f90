logical function rhyme_nombre_base_unit_chain_pow_test () result ( failed )
  use rhyme_nombre_base_unit_chain_factory
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: tester


  type ( nombre_base_unit_t ) :: bu(3)
  type ( nombre_base_unit_t ), pointer :: buc, buc_pow

  real ( kind=8 ) :: rnd(3)
  integer :: i

  integer :: ipow
  real ( kind=4 ) :: rpow
  real ( kind=8 ) :: r8pow

  tester = .describe. "nombre_base_unit_chain_pow"

  do i = 1, 5
    call random_number( rnd )

    bu = si_base_units( ceiling( rnd * size( si_base_units ) ) )
    buc => nom_buc_factory%generate( bu )

    ipow = int( rnd(1) * 100 )
    buc_pow => buc**ipow
    call tester%expect( buc_pow%pow .toBe. real( ipow, kind=8 ) )
    call tester%expect( buc_pow%next%pow .toBe. real( ipow, kind=8 ) )
    call tester%expect( buc_pow%next%next%pow .toBe. real( ipow, kind=8 ) )

    rpow = real( rnd(2) * 100, kind=4 )
    buc_pow => buc**rpow
    call tester%expect( buc_pow%pow .toBe. real( rpow, kind=8 ) )
    call tester%expect( buc_pow%next%pow .toBe. real( rpow, kind=8 ) )
    call tester%expect( buc_pow%next%next%pow .toBe. real( rpow, kind=8 ) )

    r8pow = rnd(3) * 100
    buc_pow => buc**r8pow
    call tester%expect( buc_pow%pow .toBe. r8pow )
    call tester%expect( buc_pow%next%pow .toBe. r8pow )
    call tester%expect( buc_pow%next%next%pow .toBe. r8pow )
  end do

  failed = tester%failed()
end function rhyme_nombre_base_unit_chain_pow_test
