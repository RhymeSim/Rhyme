submodule(rhyme_assertion) rhyme_assertion_within_submodule
contains
pure module function rhyme_assertion_within(test, accuracy) result(ntest)
   implicit none

   type(test_t), intent(in) :: test
   class(*), intent(in) :: accuracy

   type(test_t) :: ntest
   integer :: power

   call test%copy_essentials_to(ntest)

   select type (acc=>accuracy)
   type is (integer) ! Significant figures
      power = int(log10(abs(ntest%real_val)))
      ntest%within = 1.d1**(power - acc + 1)

      if (ntest%real_accuracy > ntest%within) then
         ntest%is_passed = .false.
      else
         ntest%is_passed = .true.
      end if

   type is (real(kind=4))
      ntest%within = acc
      if (test%real_accuracy > acc) then
         ntest%is_passed = .false.
      else
         ntest%is_passed = .true.
      end if

   type is (real(kind=8))
      ntest%within = acc
      if (test%real_accuracy > acc) then
         ntest%is_passed = .false.
      else
         ntest%is_passed = .true.
      end if
   end select
end function rhyme_assertion_within
end submodule rhyme_assertion_within_submodule
