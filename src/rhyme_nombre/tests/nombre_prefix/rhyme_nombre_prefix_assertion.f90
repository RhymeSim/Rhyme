module rhyme_nombre_prefix_assertion
  use rhyme_nombre_prefix
  use rhyme_assertion

  implicit none

  interface operator ( .toBe. )
    module procedure rhyme_nombre_prefix_assertion_tobe
  end interface operator ( .toBe. )

contains
  pure module function rhyme_nombre_prefix_assertion_tobe ( p1, p2 ) result ( test )
    implicit none

    type ( nombre_prefix_t ), intent ( in ) :: p1, p2
    type ( test_t ) :: test

    test%op = 'to_be'

    write( test%val, * ) p1
    write( test%exp, * ) p2

    test%is_passed = ( p1 == p2 )
  end function rhyme_nombre_prefix_assertion_tobe
end module rhyme_nombre_prefix_assertion
