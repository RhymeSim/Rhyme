submodule(rhyme_assertion) rhyme_assertion_not_to_be_array_scalar_sub
contains
pure module function rhyme_assertion_not_to_be_array_scalar_ii(arr, scalar) result(test)
   implicit none

   integer, intent(in) :: arr(:), scalar
   type(test_t) :: test

   type(test_t) :: temp

   temp = rhyme_assertion_to_be_array_scalar_ii(arr, scalar)
   call temp%copy_to(test)

   test%is_passed = .not. temp%is_passed
   test%op = 'not_to_be'
end function rhyme_assertion_not_to_be_array_scalar_ii

pure module function rhyme_assertion_not_to_be_array_scalar_ir(arr, scalar) result(test)
   implicit none

   integer, intent(in) :: arr(:)
   real(kind=4), intent(in) :: scalar
   type(test_t) :: test

   type(test_t) :: temp

   temp = rhyme_assertion_to_be_array_scalar_ir(arr, scalar)
   call temp%copy_to(test)

   test%is_passed = .not. temp%is_passed
   test%op = 'not_to_be'
end function rhyme_assertion_not_to_be_array_scalar_ir

pure module function rhyme_assertion_not_to_be_array_scalar_ir8(arr, scalar) result(test)
   implicit none

   integer, intent(in) :: arr(:)
   real(kind=8), intent(in) :: scalar
   type(test_t) :: test

   type(test_t) :: temp

   temp = rhyme_assertion_to_be_array_scalar_ir8(arr, scalar)
   call temp%copy_to(test)

   test%is_passed = .not. temp%is_passed
   test%op = 'not_to_be'
end function rhyme_assertion_not_to_be_array_scalar_ir8

pure module function rhyme_assertion_not_to_be_array_scalar_ri(arr, scalar) result(test)
   implicit none

   real(kind=4), intent(in) :: arr(:)
   integer, intent(in) :: scalar
   type(test_t) :: test

   type(test_t) :: temp

   temp = rhyme_assertion_to_be_array_scalar_ri(arr, scalar)
   call temp%copy_to(test)

   test%is_passed = .not. temp%is_passed
   test%op = 'not_to_be'
end function rhyme_assertion_not_to_be_array_scalar_ri

pure module function rhyme_assertion_not_to_be_array_scalar_rr(arr, scalar) result(test)
   implicit none

   real(kind=4), intent(in) :: arr(:), scalar
   type(test_t) :: test

   type(test_t) :: temp

   temp = rhyme_assertion_to_be_array_scalar_rr(arr, scalar)
   call temp%copy_to(test)

   test%is_passed = .not. temp%is_passed
   test%op = 'not_to_be'
end function rhyme_assertion_not_to_be_array_scalar_rr

pure module function rhyme_assertion_not_to_be_array_scalar_rr8(arr, scalar) result(test)
   implicit none

   real(kind=4), intent(in) :: arr(:)
   real(kind=8), intent(in) :: scalar
   type(test_t) :: test

   type(test_t) :: temp

   temp = rhyme_assertion_to_be_array_scalar_rr8(arr, scalar)
   call temp%copy_to(test)

   test%is_passed = .not. temp%is_passed
   test%op = 'not_to_be'
end function rhyme_assertion_not_to_be_array_scalar_rr8

pure module function rhyme_assertion_not_to_be_array_scalar_r8i(arr, scalar) result(test)
   implicit none

   real(kind=8), intent(in) :: arr(:)
   integer, intent(in) :: scalar
   type(test_t) :: test

   type(test_t) :: temp

   temp = rhyme_assertion_to_be_array_scalar_r8i(arr, scalar)
   call temp%copy_to(test)

   test%is_passed = .not. temp%is_passed
   test%op = 'not_to_be'
end function rhyme_assertion_not_to_be_array_scalar_r8i

pure module function rhyme_assertion_not_to_be_array_scalar_r8r(arr, scalar) result(test)
   implicit none

   real(kind=8), intent(in) :: arr(:)
   real(kind=4), intent(in) :: scalar
   type(test_t) :: test

   type(test_t) :: temp

   temp = rhyme_assertion_to_be_array_scalar_r8r(arr, scalar)
   call temp%copy_to(test)

   test%is_passed = .not. temp%is_passed
   test%op = 'not_to_be'
end function rhyme_assertion_not_to_be_array_scalar_r8r

pure module function rhyme_assertion_not_to_be_array_scalar_r8r8(arr, scalar) result(test)
   implicit none

   real(kind=8), intent(in) :: arr(:), scalar
   type(test_t) :: test

   type(test_t) :: temp

   temp = rhyme_assertion_to_be_array_scalar_r8r8(arr, scalar)
   call temp%copy_to(test)

   test%is_passed = .not. temp%is_passed
   test%op = 'not_to_be'
end function rhyme_assertion_not_to_be_array_scalar_r8r8

pure module function rhyme_assertion_not_to_be_array_scalar_chch(arr, scalar) result(test)
   implicit none

   character(len=*), intent(in) :: arr(:), scalar
   type(test_t) :: test

   type(test_t) :: temp

   temp = rhyme_assertion_to_be_array_scalar_chch(arr, scalar)
   call temp%copy_to(test)

   test%is_passed = .not. temp%is_passed
   test%op = 'not_to_be'
end function rhyme_assertion_not_to_be_array_scalar_chch

pure module function rhyme_assertion_not_to_be_array_scalar_ll(arr, scalar) result(test)
   implicit none

   logical, intent(in) :: arr(:), scalar
   type(test_t) :: test

   type(test_t) :: temp

   temp = rhyme_assertion_to_be_array_scalar_ll(arr, scalar)
   call temp%copy_to(test)

   test%is_passed = .not. temp%is_passed
   test%op = 'not_to_be'
end function rhyme_assertion_not_to_be_array_scalar_ll
end submodule rhyme_assertion_not_to_be_array_scalar_sub
