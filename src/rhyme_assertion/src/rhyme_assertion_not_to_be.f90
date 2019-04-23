submodule ( rhyme_assertion ) rhyme_assertion_not_to_be_submodule
contains
  pure module function rhyme_assertion_not_to_be ( val, exp ) result ( test )
    implicit none

    class (*), intent ( in ) :: val, exp
    type ( test_t ) :: test

    type ( test_t ) :: temp

    temp = rhyme_assertion_to_be( val, exp )

    call temp%copy_to( test )

    test%is_passed = .not. temp%is_passed
    test%op = 'not_to_be'
  end function rhyme_assertion_not_to_be
end submodule rhyme_assertion_not_to_be_submodule
