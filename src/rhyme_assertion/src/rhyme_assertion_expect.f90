submodule(rhyme_assertion) rhyme_assertion_expect_submodule
contains
pure module subroutine rhyme_assertion_expect(this, test)
   implicit none

   class(assertion_t), intent(inout) :: this
   type(test_t), intent(in) :: test

   type(test_t), pointer :: t_ptr

   t_ptr => this%tail

   if (associated(t_ptr)) then
      do while (associated(t_ptr%next))
         t_ptr => t_ptr%next
      end do

      allocate (t_ptr%next)
      t_ptr => t_ptr%next
   else
      allocate (this%tests)
      t_ptr => this%tests
   end if

   this%tail => t_ptr

   t_ptr%msg = test%msg
   t_ptr%type = test%type
   t_ptr%is_passed = test%is_passed
   t_ptr%val = test%val
   t_ptr%op = test%op
   t_ptr%exp = test%exp
end subroutine rhyme_assertion_expect
end submodule rhyme_assertion_expect_submodule
