module rhyme_nombre_base_unit_assertion
  use rhyme_nombre_base_unit
  use rhyme_assertion

  implicit none

  interface operator ( .toBe. )
    module procedure rhyme_nombre_base_unit_assertion_tobe
  end interface operator ( .toBe. )

contains

  pure module function rhyme_nombre_base_unit_assertion_tobe ( bu_1, bu_2 ) result ( test )
    implicit none

    type ( nombre_base_unit_t ), intent ( in ) :: bu_1, bu_2
    type ( test_t ) :: test

    test%op = 'to_be'

    write( test%val, * ) bu_1
    write( test%exp, * ) bu_2

    test%is_passed = bu_1 == bu_1
  end function rhyme_nombre_base_unit_assertion_tobe
end module rhyme_nombre_base_unit_assertion
