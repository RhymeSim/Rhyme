submodule(rhyme_assertion) rhyme_assertion_not_to_be_array_submodule
contains
   pure module function rhyme_assertion_not_to_be_array_ii(arr1, arr2) result(test)
      implicit none

      integer, intent(in) :: arr1(:), arr2(:)
      type(test_t) :: test

      type(test_t) :: temp

      temp = rhyme_assertion_to_be_array_ii(arr1, arr2)
      call temp%copy_to(test)

      test%is_passed = .not. temp%is_passed
      test%op = 'not_to_be'
   end function rhyme_assertion_not_to_be_array_ii

   pure module function rhyme_assertion_not_to_be_array_ir(arr1, arr2) result(test)
      implicit none

      integer, intent(in) :: arr1(:)
      real(kind=4), intent(in) :: arr2(:)
      type(test_t) :: test

      type(test_t) :: temp

      temp = rhyme_assertion_to_be_array_ir(arr1, arr2)
      call temp%copy_to(test)

      test%is_passed = .not. temp%is_passed
      test%op = 'not_to_be'
   end function rhyme_assertion_not_to_be_array_ir

   pure module function rhyme_assertion_not_to_be_array_ir8(arr1, arr2) result(test)
      implicit none

      integer, intent(in) :: arr1(:)
      real(kind=8), intent(in) :: arr2(:)
      type(test_t) :: test

      type(test_t) :: temp

      temp = rhyme_assertion_to_be_array_ir8(arr1, arr2)
      call temp%copy_to(test)

      test%is_passed = .not. temp%is_passed
      test%op = 'not_to_be'
   end function rhyme_assertion_not_to_be_array_ir8

   pure module function rhyme_assertion_not_to_be_array_ri(arr1, arr2) result(test)
      implicit none

      real(kind=4), intent(in) :: arr1(:)
      integer, intent(in) :: arr2(:)
      type(test_t) :: test

      type(test_t) :: temp

      temp = rhyme_assertion_to_be_array_ri(arr1, arr2)
      call temp%copy_to(test)

      test%is_passed = .not. temp%is_passed
      test%op = 'not_to_be'
   end function rhyme_assertion_not_to_be_array_ri

   pure module function rhyme_assertion_not_to_be_array_rr(arr1, arr2) result(test)
      implicit none

      real(kind=4), intent(in) :: arr1(:), arr2(:)
      type(test_t) :: test

      type(test_t) :: temp

      temp = rhyme_assertion_to_be_array_rr(arr1, arr2)
      call temp%copy_to(test)

      test%is_passed = .not. temp%is_passed
      test%op = 'not_to_be'
   end function rhyme_assertion_not_to_be_array_rr

   pure module function rhyme_assertion_not_to_be_array_rr8(arr1, arr2) result(test)
      implicit none

      real(kind=4), intent(in) :: arr1(:)
      real(kind=8), intent(in) :: arr2(:)
      type(test_t) :: test

      type(test_t) :: temp

      temp = rhyme_assertion_to_be_array_rr8(arr1, arr2)
      call temp%copy_to(test)

      test%is_passed = .not. temp%is_passed
      test%op = 'not_to_be'
   end function rhyme_assertion_not_to_be_array_rr8

   pure module function rhyme_assertion_not_to_be_array_r8i(arr1, arr2) result(test)
      implicit none

      real(kind=8), intent(in) :: arr1(:)
      integer, intent(in) :: arr2(:)
      type(test_t) :: test

      type(test_t) :: temp

      temp = rhyme_assertion_to_be_array_r8i(arr1, arr2)
      call temp%copy_to(test)

      test%is_passed = .not. temp%is_passed
      test%op = 'not_to_be'
   end function rhyme_assertion_not_to_be_array_r8i

   pure module function rhyme_assertion_not_to_be_array_r8r(arr1, arr2) result(test)
      implicit none

      real(kind=8), intent(in) :: arr1(:)
      real(kind=4), intent(in) :: arr2(:)
      type(test_t) :: test

      type(test_t) :: temp

      temp = rhyme_assertion_to_be_array_r8r(arr1, arr2)
      call temp%copy_to(test)

      test%is_passed = .not. temp%is_passed
      test%op = 'not_to_be'
   end function rhyme_assertion_not_to_be_array_r8r

   pure module function rhyme_assertion_not_to_be_array_r8r8(arr1, arr2) result(test)
      implicit none

      real(kind=8), intent(in) :: arr1(:), arr2(:)
      type(test_t) :: test

      type(test_t) :: temp

      temp = rhyme_assertion_to_be_array_r8r8(arr1, arr2)
      call temp%copy_to(test)

      test%is_passed = .not. temp%is_passed
      test%op = 'not_to_be'
   end function rhyme_assertion_not_to_be_array_r8r8

   pure module function rhyme_assertion_not_to_be_array_chch(arr1, arr2) result(test)
      implicit none

      character(len=*), intent(in) :: arr1(:), arr2(:)
      type(test_t) :: test

      type(test_t) :: temp

      temp = rhyme_assertion_to_be_array_chch(arr1, arr2)
      call temp%copy_to(test)

      test%is_passed = .not. temp%is_passed
      test%op = 'not_to_be'
   end function rhyme_assertion_not_to_be_array_chch

   pure module function rhyme_assertion_not_to_be_array_ll(arr1, arr2) result(test)
      implicit none

      logical, intent(in) :: arr1(:), arr2(:)
      type(test_t) :: test

      type(test_t) :: temp

      temp = rhyme_assertion_to_be_array_ll(arr1, arr2)
      call temp%copy_to(test)

      test%is_passed = .not. temp%is_passed
      test%op = 'not_to_be'
   end function rhyme_assertion_not_to_be_array_ll
end submodule rhyme_assertion_not_to_be_array_submodule
