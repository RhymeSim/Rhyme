submodule ( rhyme_assertion ) rhyme_assertion_not_to_be_submodule
contains
  pure module function rhyme_assertion_not_to_be_ii ( val, exp ) result ( test )
    implicit none

    integer, intent ( in ) :: val, exp
    type ( test_t ) :: test

    type ( test_t ) :: temp

    temp = rhyme_assertion_to_be_ii( val, exp )
    call temp%copy_to( test )

    test%op = 'not_to_be'
    test%is_passed = .not. test%is_passed
  end function rhyme_assertion_not_to_be_ii

  pure module function rhyme_assertion_not_to_be_ir ( val, exp ) result ( test )
    implicit none

    integer, intent ( in ) :: val
    real ( kind=4 ), intent ( in ) :: exp
    type ( test_t ) :: test

    type ( test_t ) :: temp

    temp = rhyme_assertion_to_be_ir( val, exp )
    call temp%copy_to( test )

    test%op = 'not_to_be'
    test%is_passed = .not. test%is_passed
  end function rhyme_assertion_not_to_be_ir

  pure module function rhyme_assertion_not_to_be_ir8 ( val, exp ) result ( test )
    implicit none

    integer, intent ( in ) :: val
    real ( kind=8 ) , intent ( in ):: exp
    type ( test_t ) :: test

    type ( test_t ) :: temp

    temp = rhyme_assertion_to_be_ir8( val, exp )
    call temp%copy_to( test )

    test%op = 'not_to_be'
    test%is_passed = .not. test%is_passed
  end function rhyme_assertion_not_to_be_ir8

  pure function rhyme_assertion_not_to_be_ri ( val, exp ) result ( test )
    implicit none

    real ( kind=4 ), intent ( in ) :: val
    integer, intent ( in ) :: exp
    type ( test_t ) :: test

    type ( test_t ) :: temp

    temp = rhyme_assertion_to_be_ri( val, exp )
    call temp%copy_to( test )

    test%op = 'not_to_be'
    test%is_passed = .not. test%is_passed
  end function rhyme_assertion_not_to_be_ri

  pure function rhyme_assertion_not_to_be_rr ( val, exp ) result ( test )
    implicit none

    real ( kind=4 ), intent ( in ) :: val, exp
    type ( test_t ) :: test

    type ( test_t ) :: temp

    temp = rhyme_assertion_to_be_rr( val, exp )
    call temp%copy_to( test )

    test%op = 'not_to_be'
    test%is_passed = .not. test%is_passed
  end function rhyme_assertion_not_to_be_rr

  pure function rhyme_assertion_not_to_be_rr8 ( val, exp ) result ( test )
    implicit none

    real ( kind=4 ), intent ( in ) :: val
    real ( kind=8 ), intent ( in ) :: exp
    type ( test_t ) :: test

    type ( test_t ) :: temp

    temp = rhyme_assertion_to_be_rr8( val, exp )
    call temp%copy_to( test )

    test%op = 'not_to_be'
    test%is_passed = .not. test%is_passed
  end function rhyme_assertion_not_to_be_rr8

  pure function rhyme_assertion_not_to_be_r8i ( val, exp ) result ( test )
    implicit none

    real ( kind=8 ), intent ( in ) :: val
    integer, intent ( in ) :: exp
    type ( test_t ) :: test

    type ( test_t ) :: temp

    temp = rhyme_assertion_to_be_r8i( val, exp )
    call temp%copy_to( test )

    test%op = 'not_to_be'
    test%is_passed = .not. test%is_passed
  end function rhyme_assertion_not_to_be_r8i

  pure function rhyme_assertion_not_to_be_r8r ( val, exp ) result ( test )
    implicit none

    real ( kind=8 ), intent ( in ) :: val
    real ( kind=4 ), intent ( in ) :: exp
    type ( test_t ) :: test

    type ( test_t ) :: temp

    temp = rhyme_assertion_to_be_r8r( val, exp )
    call temp%copy_to( test )

    test%op = 'not_to_be'
    test%is_passed = .not. test%is_passed
  end function rhyme_assertion_not_to_be_r8r

  pure function rhyme_assertion_not_to_be_r8r8 ( val, exp ) result ( test )
    implicit none

    real ( kind=8 ), intent ( in ) :: val, exp
    type ( test_t ) :: test

    type ( test_t ) :: temp

    temp = rhyme_assertion_to_be_r8r8( val, exp )
    call temp%copy_to( test )

    test%op = 'not_to_be'
    test%is_passed = .not. test%is_passed
  end function rhyme_assertion_not_to_be_r8r8

  pure function rhyme_assertion_not_to_be_chch ( val, exp ) result ( test )
    implicit none

    character ( len=* ), intent ( in ) :: val, exp
    type ( test_t ) :: test

    type ( test_t ) :: temp

    temp = rhyme_assertion_to_be_chch( val, exp )
    call temp%copy_to( test )

    test%op = 'not_to_be'
    test%is_passed = .not. test%is_passed
  end function rhyme_assertion_not_to_be_chch

  pure function rhyme_assertion_not_to_be_ll ( val, exp ) result ( test )
    implicit none

    logical, intent ( in ) :: val, exp
    type ( test_t ) :: test

    type ( test_t ) :: temp

    temp = rhyme_assertion_to_be_ll( val, exp )
    call temp%copy_to( test )

    test%op = 'not_to_be'
    test%is_passed = .not. test%is_passed
  end function rhyme_assertion_not_to_be_ll
end submodule rhyme_assertion_not_to_be_submodule
