submodule(rhyme_assertion) rhyme_assertion_to_be_array_3d_scalar_submodule
contains
pure module function rhyme_assertion_to_be_array_3d_scalar_ii(arr, scalar) result(test)
   implicit none

   integer, intent(in) :: arr(:, :, :), scalar
   type(test_t) :: test

   type(test_t) :: temp
   integer :: l

   l = product(shape(arr))
   temp = rhyme_assertion_to_be_array_scalar_ii(reshape(arr, [l]), scalar)
   call temp%copy_to(test)
end function rhyme_assertion_to_be_array_3d_scalar_ii

pure module function rhyme_assertion_to_be_array_3d_scalar_ir(arr, scalar) result(test)
   implicit none

   integer, intent(in) :: arr(:, :, :)
   real(kind=4), intent(in) :: scalar
   type(test_t) :: test

   type(test_t) :: temp
   integer :: l

   l = product(shape(arr))
   temp = rhyme_assertion_to_be_array_scalar_ir(reshape(arr, [l]), scalar)
   call temp%copy_to(test)
end function rhyme_assertion_to_be_array_3d_scalar_ir

pure module function rhyme_assertion_to_be_array_3d_scalar_ir8(arr, scalar) result(test)
   implicit none

   integer, intent(in) :: arr(:, :, :)
   real(kind=8), intent(in) :: scalar
   type(test_t) :: test

   type(test_t) :: temp
   integer :: l

   l = product(shape(arr))
   temp = rhyme_assertion_to_be_array_scalar_ir8(reshape(arr, [l]), scalar)
   call temp%copy_to(test)
end function rhyme_assertion_to_be_array_3d_scalar_ir8

pure module function rhyme_assertion_to_be_array_3d_scalar_ri(arr, scalar) result(test)
   implicit none

   real(kind=4), intent(in) :: arr(:, :, :)
   integer, intent(in) :: scalar
   type(test_t) :: test

   type(test_t) :: temp
   integer :: l

   l = product(shape(arr))
   temp = rhyme_assertion_to_be_array_scalar_ri(reshape(arr, [l]), scalar)
   call temp%copy_to(test)
end function rhyme_assertion_to_be_array_3d_scalar_ri

pure module function rhyme_assertion_to_be_array_3d_scalar_rr(arr, scalar) result(test)
   implicit none

   real(kind=4), intent(in) :: arr(:, :, :), scalar
   type(test_t) :: test

   type(test_t) :: temp
   integer :: l

   l = product(shape(arr))
   temp = rhyme_assertion_to_be_array_scalar_rr(reshape(arr, [l]), scalar)
   call temp%copy_to(test)
end function rhyme_assertion_to_be_array_3d_scalar_rr

pure module function rhyme_assertion_to_be_array_3d_scalar_rr8(arr, scalar) result(test)
   implicit none

   real(kind=4), intent(in) :: arr(:, :, :)
   real(kind=8), intent(in) :: scalar
   type(test_t) :: test

   type(test_t) :: temp
   integer :: l

   l = product(shape(arr))
   temp = rhyme_assertion_to_be_array_scalar_rr8(reshape(arr, [l]), scalar)
   call temp%copy_to(test)
end function rhyme_assertion_to_be_array_3d_scalar_rr8

pure module function rhyme_assertion_to_be_array_3d_scalar_r8i(arr, scalar) result(test)
   implicit none

   real(kind=8), intent(in) :: arr(:, :, :)
   integer, intent(in) :: scalar
   type(test_t) :: test

   type(test_t) :: temp
   integer :: l

   l = product(shape(arr))
   temp = rhyme_assertion_to_be_array_scalar_r8i(reshape(arr, [l]), scalar)
   call temp%copy_to(test)
end function rhyme_assertion_to_be_array_3d_scalar_r8i

pure module function rhyme_assertion_to_be_array_3d_scalar_r8r(arr, scalar) result(test)
   implicit none

   real(kind=8), intent(in) :: arr(:, :, :)
   real(kind=4), intent(in) :: scalar
   type(test_t) :: test

   type(test_t) :: temp
   integer :: l

   l = product(shape(arr))
   temp = rhyme_assertion_to_be_array_scalar_r8r(reshape(arr, [l]), scalar)
   call temp%copy_to(test)
end function rhyme_assertion_to_be_array_3d_scalar_r8r

pure module function rhyme_assertion_to_be_array_3d_scalar_r8r8(arr, scalar) result(test)
   implicit none

   real(kind=8), intent(in) :: arr(:, :, :), scalar
   type(test_t) :: test

   type(test_t) :: temp
   integer :: l

   l = product(shape(arr))
   temp = rhyme_assertion_to_be_array_scalar_r8r8(reshape(arr, [l]), scalar)
   call temp%copy_to(test)
end function rhyme_assertion_to_be_array_3d_scalar_r8r8

pure module function rhyme_assertion_to_be_array_3d_scalar_chch(arr, scalar) result(test)
   implicit none

   character(len=*), intent(in) :: arr(:, :, :), scalar
   type(test_t) :: test

   type(test_t) :: temp
   integer :: l

   l = product(shape(arr))
   temp = rhyme_assertion_to_be_array_scalar_chch(reshape(arr, [l]), scalar)
   call temp%copy_to(test)
end function rhyme_assertion_to_be_array_3d_scalar_chch

pure module function rhyme_assertion_to_be_array_3d_scalar_ll(arr, scalar) result(test)
   implicit none

   logical, intent(in) :: arr(:, :, :), scalar
   type(test_t) :: test

   type(test_t) :: temp
   integer :: l

   l = product(shape(arr))
   temp = rhyme_assertion_to_be_array_scalar_ll(reshape(arr, [l]), scalar)
   call temp%copy_to(test)
end function rhyme_assertion_to_be_array_3d_scalar_ll
end submodule rhyme_assertion_to_be_array_3d_scalar_submodule
