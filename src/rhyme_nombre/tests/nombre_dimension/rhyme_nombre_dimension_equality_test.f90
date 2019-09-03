logical function rhyme_nombre_dimension_equality_test () result ( failed )
  use rhyme_nombre_dimension
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: tester

  type ( nombre_dimension_t ) :: d1, d2

  tester = .describe. "nombre_dimension_equality"

  d1%powers = [ 1, 2, 3, 4, 5, 6, 7 ]
  d2%powers = [ 1, 2, 3, 4, 5, 6, 7 ]

  call tester%expect( rhyme_nombre_dimension_equality( d1, d2 ) .toBe. .true. )
  call tester%expect( d1 == d1 .toBe. .true. )
  call tester%expect( d2 == d2 .toBe. .true. )
  call tester%expect( d1 == d2 .toBe. .true. )
  call tester%expect( d2 == d1 .toBe. .true. )

  d2%powers = [ 1, 2, 3, 4, 5, 6, 6 ]
  call tester%expect( d1 == d2 .toBe. .false. )
  call tester%expect( d2 == d1 .toBe. .false. )

  failed = tester%failed()
end function rhyme_nombre_dimension_equality_test
