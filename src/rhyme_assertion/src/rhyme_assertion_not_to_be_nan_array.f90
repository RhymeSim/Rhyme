submodule(rhyme_assertion) rhyme_assertion_not_to_be_nan_array_submodule
contains
pure module function rhyme_assertion_not_to_be_nan_array(input) result(test)
   use, intrinsic :: ieee_arithmetic

   implicit none

   class(*), intent(in) :: input(:)
   type(test_t) :: test

   test%op = 'not_to_be_nan'

   test%real_exp = ieee_value(test%real_exp, ieee_quiet_nan)
   test%real_accuracy = ieee_value(test%real_accuracy, ieee_quiet_nan)
   test%within = ieee_value(test%within, ieee_quiet_nan)

   test%val = .toString.input
   test%exp = 'NaN'

   call test%set_type(input(1))

   if (any(.isNaN.input)) then
      test%is_passed = .false.
   else
      test%is_passed = .true.
   end if

end function rhyme_assertion_not_to_be_nan_array
end submodule rhyme_assertion_not_to_be_nan_array_submodule
