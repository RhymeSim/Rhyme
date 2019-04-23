submodule ( rhyme_assertion ) rhyme_assertion_within_submodule
contains
  pure module function rhyme_assertion_within ( test, accuracy ) result ( ntest )
    implicit none

    type ( test_t ), intent ( in ) :: test
    class (*), intent ( in ) :: accuracy

    type ( test_t ) :: ntest

    ntest%type = test%type
    ntest%is_passed = test%is_passed
    ntest%msg = test%msg
    ntest%val = test%val
    ntest%op = test%op
    ntest%exp = test%exp
    ntest%within = test%within
    ntest%real_accuracy = test%real_accuracy

    select type ( acc => accuracy )
    type is ( real( kind=4 ) )
      ntest%within = acc
      if ( test%real_accuracy < acc ) ntest%is_passed = .true.
    type is ( real( kind=8 ) )
      ntest%within = acc
      if ( test%real_accuracy < acc ) ntest%is_passed = .true.
    end select
  end function rhyme_assertion_within
end submodule rhyme_assertion_within_submodule
