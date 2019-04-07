submodule ( rhyme_assertion ) rhyme_assertion_to_be_submodule
contains
  pure module function rhyme_assertion_to_be ( val, exp ) result ( test )
    implicit none

    class (*), intent ( in ) :: val, exp
    type ( test_t ) :: test

    logical :: passed
    passed = .false.

    select type ( v => val )
    type is ( integer )
      test%type = assertid%int

      select type ( e => exp )
      type is ( integer )
        passed = v .eq. e
      class default
        passed = .false.
      end select

    end select

    test%is_passed = passed
  end function rhyme_assertion_to_be
end submodule rhyme_assertion_to_be_submodule
