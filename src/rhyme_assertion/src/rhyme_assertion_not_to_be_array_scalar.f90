submodule ( rhyme_assertion ) rhyme_assertion_not_to_be_array_scalar_sub
contains
  pure module function rhyme_assertion_not_to_be_array_scalar ( val, expect ) result ( test )
    implicit none

    class (*), intent ( in ) :: val(:), expect
    type ( test_t ) :: test

    type ( test_t ) :: temp

    temp = rhyme_assertion_to_be_array_scalar( val, expect )

    call temp%copy_to( test )

    test%is_passed = .not. temp%is_passed
    test%op = 'not_to_be'
  end function rhyme_assertion_not_to_be_array_scalar
end submodule rhyme_assertion_not_to_be_array_scalar_sub
