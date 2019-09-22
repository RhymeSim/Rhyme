module rhyme_nombre_assertion
  use rhyme_nombre_unit_assertion
  use rhyme_nombre
  use rhyme_assertion

  implicit none

  interface operator ( .toBe. )
    module procedure rhyme_nombre_assertion_tobe
  end interface operator ( .toBe. )

contains

  module function rhyme_nombre_assertion_tobe ( n_1, n_2 ) result ( test )
    implicit none

    type ( nombre_t ), intent ( in ) :: n_1, n_2
    type ( test_t ) :: test

    test%op = 'to_be'

    write( test%val, * ) n_1
    write( test%exp, * ) n_2

    test%is_passed = n_1 == n_2
  end function rhyme_nombre_assertion_tobe
end module rhyme_nombre_assertion
