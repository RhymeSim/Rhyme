submodule ( rhyme_assertion ) rhyme_assertion_describe_submodule
contains
  pure module function rhyme_assertion_describe ( desc ) result ( tester )
    implicit none

    character ( len=* ), intent ( in ) :: desc
    type ( assertion_t ) :: tester

    tester%desc = trim( adjustl( desc ) )
  end function rhyme_assertion_describe
end submodule rhyme_assertion_describe_submodule
