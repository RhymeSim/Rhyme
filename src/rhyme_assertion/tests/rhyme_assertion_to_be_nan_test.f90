logical function rhyme_assertion_to_be_nan_test() result(failed)
   use rhyme_assertion
   use, intrinsic :: ieee_arithmetic

   implicit none

   type(test_t) :: test
   real(kind=4) :: r4
   real(kind=8) :: r8
   integer :: i

   r4 = 1.23e4
   test = .toBeNaN.r4

   failed = test%is_passed &
            .or. test%type .ne. assertid%real &
            .or. test%op .ne. 'to_be_nan' &
            .or. .not. ieee_is_nan(test%real_exp) &
            .or. .not. ieee_is_nan(test%real_accuracy) &
            .or. .not. ieee_is_nan(test%within) &
            .or. abs(test%real_val - 1.23e4) > epsilon(0.e0)
   if (failed) return

   r8 = 1.23d4
   test = .toBeNaN.r8

   failed = test%is_passed &
            .or. test%type .ne. assertid%double &
            .or. abs(test%real_val - 1.23d4) > epsilon(0.d0)
   if (failed) return

   r4 = ieee_value(r4, ieee_quiet_nan)
   test = .toBeNaN.r4

   failed = .not. test%is_passed &
            .or. test%type .ne. assertid%real &
            .or. test%val .ne. 'NaN' &
            .or. .not. ieee_is_nan(test%real_val)
   if (failed) return

   r8 = ieee_value(r8, ieee_quiet_nan)
   test = .toBeNaN.r8

   failed = .not. test%is_passed &
            .or. test%type .ne. assertid%double &
            .or. test%val .ne. 'NaN' &
            .or. .not. ieee_is_nan(test%real_val)
   if (failed) return

   i = 1234
   test = .toBeNaN.i

   failed = test%is_passed &
            .or. test%type .ne. assertid%int &
            .or. test%val .ne. '1234'
   if (failed) return
end function rhyme_assertion_to_be_nan_test
