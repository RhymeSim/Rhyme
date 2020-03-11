submodule(rhyme_assertion) rhyme_assertion_failed_submodule
contains
logical module function rhyme_assertion_failed(this) result(failed)
   implicit none

   class(assertion_t), intent(in) :: this

   failed = .not. this%passed()
end function rhyme_assertion_failed
end submodule rhyme_assertion_failed_submodule
