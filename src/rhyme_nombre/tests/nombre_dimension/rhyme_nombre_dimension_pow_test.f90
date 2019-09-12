logical function rhyme_nombre_dimension_pow_test () result ( failed )
  use rhyme_nombre_dimension
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: tester
  type ( nombre_dimension_t ) :: i1, i2, r1, r2, r81, r82

  tester = .describe. "nombre_dimension_pow"

  i1%powers = -1 * dimid%mass%powers + 5d-1 * dimid%length%powers + 2 * dimid%time%powers

  i2 = i1**1
  call tester%expect( i2%powers .toBe. [ -1d0, 5d-1, 2d0, 0d0, 0d0, 0d0, 0d0 ] .hint. '**1' )

  i2 = i1**2
  call tester%expect( i2%powers .toBe. [ -2d0, 1d0, 4d0, 0d0, 0d0, 0d0, 0d0 ] .hint. '**2' )

  i2 = i1**(-3)
  call tester%expect( i2%powers .toBe. [ 3d0, -1.5d0, -6d0, 0d0, 0d0, 0d0, 0d0 ] .hint. '**-3' )


  r1%powers = 1.23e0 * dimid%mass%powers - 2.34e5 * dimid%length%powers

  r2 = r1**(-1.23d0)
  call tester%expect( r2%powers .toBe. [ -1.23e0 * 1.23e0, -2.34e5 * (-1.23e0), 0e0, 0e0, 0e0, 0e0, 0e0 ] .hint. '**1' )


  r81%powers = 1.23d0 * dimid%mass%powers - 2.34d5 * dimid%length%powers

  r82 = r81**(-1.23d0)
  call tester%expect( r82%powers .toBe. [ -1.23d0 * 1.23d0, -2.34d5 * (-1.23d0), 0d0, 0d0, 0d0, 0d0, 0d0 ] .hint. '**1' )

  failed = tester%failed()
end function rhyme_nombre_dimension_pow_test
