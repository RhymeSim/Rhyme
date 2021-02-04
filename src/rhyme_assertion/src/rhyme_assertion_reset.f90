submodule(rhyme_assertion) rhyme_assertion_reset_submodule
contains
   module subroutine rhyme_assertion_reset(this)
      implicit none

      class(assertion_t), intent(inout) :: this

      ! TODO: deallocate tests properly
      this%tests => null()
   end subroutine rhyme_assertion_reset
end submodule rhyme_assertion_reset_submodule
