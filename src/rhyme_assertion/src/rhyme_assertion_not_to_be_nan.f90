submodule(rhyme_assertion) rhyme_assertion_not_to_be_nan_submodule
contains
   elemental pure module function rhyme_assertion_not_to_be_nan(input) result(test)
      use, intrinsic :: ieee_arithmetic

      implicit none

      class(*), intent(in) :: input
      type(test_t) :: test

      type(test_t) :: temp

      temp = rhyme_assertion_to_be_nan(input)

      temp%op = 'not_to_be_nan'
      temp%real_exp = 0.d0
      temp%real_accuracy = 0.d0
      temp%within = 0.d0

      temp%is_passed = .not. temp%is_passed

      call temp%copy_to(test)
   end function rhyme_assertion_not_to_be_nan
end submodule rhyme_assertion_not_to_be_nan_submodule
