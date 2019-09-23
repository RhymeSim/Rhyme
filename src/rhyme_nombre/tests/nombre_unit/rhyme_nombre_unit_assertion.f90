module rhyme_nombre_unit_assertion
  use rhyme_assertion

  use rhyme_nombre_base_unit_assertion
  use rhyme_nombre_derived_unit_assertion

  implicit none

  interface operator ( .toBe. )
    module procedure rhyme_nombre_unit_assertion_tobe_buu
    module procedure rhyme_nombre_unit_assertion_tobe_ubu
  end interface operator ( .toBe. )

contains

  module function rhyme_nombre_unit_assertion_tobe_buu ( bu, u ) result ( test )
    implicit none

    type ( nombre_base_unit_t ), intent ( in ) :: bu
    type ( nombre_unit_t ), intent ( in ) :: u
    type ( test_t ) :: test

    test%op = 'to_be'

    write( test%val, * ) bu
    write( test%exp, * ) u

    test%is_passed = (1 * bu) == u
  end function rhyme_nombre_unit_assertion_tobe_buu

  module function rhyme_nombre_unit_assertion_tobe_ubu ( u, bu ) result ( test )
    implicit none

    type ( nombre_unit_t ), intent ( in ) :: u
    type ( nombre_base_unit_t ), intent ( in ) :: bu
    type ( test_t ) :: test

    test%op = 'to_be'

    write( test%val, * ) u
    write( test%exp, * ) bu

    test%is_passed = u == (1 * bu)
  end function rhyme_nombre_unit_assertion_tobe_ubu
end module rhyme_nombre_unit_assertion
