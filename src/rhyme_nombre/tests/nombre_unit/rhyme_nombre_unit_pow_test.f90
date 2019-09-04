logical function rhyme_nombre_unit_pow_test () result ( failed )
  use rhyme_nombre_unit
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: tester
  type ( nombre_unit_t ), pointer :: u

  integer :: i
  real ( kind=4 ) :: r
  real ( kind=8 ) :: r8

  tester = .describe. "nombre_unit_pow"

  do i = -67, 97, 7
    u => kg**i
    call tester%expect( u%pow .toBe. real( i, kind=8 ) )

    r = i * 1.23e0
    u => meter**r
    call tester%expect( u%pow .toBe. real( r, kind=8 ) )

    r8 = i * 2.34d0
    u => kel**r8
    call tester%expect( u%pow .toBe. r8 )
  end do

  failed = tester%failed()
end function rhyme_nombre_unit_pow_test
