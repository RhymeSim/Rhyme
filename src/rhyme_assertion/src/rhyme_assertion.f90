module rhyme_assertion
   use rhyme_string

   implicit none

   type, private :: assertion_indices_t
      integer :: nan = 0, int = 1, real = 2, double = 3, char = 4, log = 5, unknown = 6
      integer :: int_arr = 11, real_arr = 12, double_arr = 13, char_arr = 14, log_arr = 15
      integer :: int_arr2d = 21, real_arr2d = 22, double_arr2d = 23, char_arr2d = 24, log_arr2d = 25
      integer :: int_arr3d = 31, real_arr3d = 32, double_arr3d = 33, char_arr3d = 34, log_arr3d = 35
      integer :: unset = -1
   end type assertion_indices_t

   type(assertion_indices_t), parameter :: assertid = assertion_indices_t()

   type, private :: assertion_constants_t
      character(len=16) :: int_fmt = strcnst%int_fmt
      character(len=16) :: real_fmt = strcnst%real_fmt
      character(len=16) :: double_fmt = strcnst%double_fmt
   end type assertion_constants_t

   type(assertion_constants_t), parameter :: assertcnst = assertion_constants_t()

   type test_t
      integer :: type = assertid%unset
      logical :: is_passed = .false.
      character(len=128) :: msg = ''
      character(len=2048) :: val = '', op = '', exp = ''
      real(kind=8) :: within = 0.d0
      real(kind=8) :: real_accuracy = 0.d0
      real(kind=8) :: real_val = 0.d0
      real(kind=8) :: real_exp = 0.d0
      type(test_t), pointer :: next => null()
   contains
      procedure :: copy_to => rhyme_assertion_test_copy_to
      procedure :: copy_essentials_to => rhyme_assertion_test_copy_essentials_to
      procedure :: set_type => rhyme_assertion_test_set_type
      procedure :: set_real_val => rhyme_assertion_test_set_real_val
   end type test_t

   type assertion_t
      character(len=128) :: desc = ''
      type(test_t), pointer :: tests => null()
      type(test_t), pointer :: tail => null()
   contains
      procedure :: expect => rhyme_assertion_expect
      procedure :: passed => rhyme_assertion_passed
      procedure :: failed => rhyme_assertion_failed
      procedure :: reset => rhyme_assertion_reset
   end type assertion_t

   interface
      logical module function rhyme_assertion_passed(this) result(passed)
         class(assertion_t), intent(in) :: this
      end function rhyme_assertion_passed

      logical module function rhyme_assertion_failed(this) result(failed)
         class(assertion_t), intent(in) :: this
      end function rhyme_assertion_failed

      pure module function rhyme_assertion_within(test, accuracy) result(ntest)
         type(test_t), intent(in) :: test
         class(*), intent(in) :: accuracy
         type(test_t) :: ntest
      end function rhyme_assertion_within

      module subroutine rhyme_assertion_reset(this)
         class(assertion_t), intent(inout) :: this
      end subroutine rhyme_assertion_reset

      pure module function rhyme_assertion_describe(desc) result(tester)
         character(len=*), intent(in) :: desc
         type(assertion_t) :: tester
      end function rhyme_assertion_describe

      pure module subroutine rhyme_assertion_expect(this, test)
         class(assertion_t), intent(inout) :: this
         type(test_t), intent(in) :: test
      end subroutine rhyme_assertion_expect

      pure module function rhyme_assertion_add_test_message(t, msg) result(test)
         type(test_t), intent(in) :: t
         character(len=*), intent(in) :: msg
         type(test_t) :: test
      end function rhyme_assertion_add_test_message

      elemental pure module function rhyme_assertion_to_be_nan(input) result(test)
         class(*), intent(in) :: input
         type(test_t) :: test
      end function rhyme_assertion_to_be_nan

      elemental pure module function rhyme_assertion_not_to_be_nan(input) result(test)
         class(*), intent(in) :: input
         type(test_t) :: test
      end function rhyme_assertion_not_to_be_nan

      pure module function rhyme_assertion_to_be_nan_array(input) result(test)
         class(*), intent(in) :: input(:)
         type(test_t) :: test
      end function rhyme_assertion_to_be_nan_array

      pure module function rhyme_assertion_not_to_be_nan_array(input) result(test)
         class(*), intent(in) :: input(:)
         type(test_t) :: test
      end function rhyme_assertion_not_to_be_nan_array

      pure module function rhyme_assertion_to_be_ii(val, exp) result(test)
         integer, intent(in) :: val, exp
         type(test_t) :: test
      end function rhyme_assertion_to_be_ii

      pure module function rhyme_assertion_to_be_ir(val, exp) result(test)
         integer, intent(in) :: val
         real(kind=4), intent(in) :: exp
         type(test_t) :: test
      end function rhyme_assertion_to_be_ir

      pure module function rhyme_assertion_to_be_ir8(val, exp) result(test)
         integer, intent(in) :: val
         real(kind=8), intent(in) :: exp
         type(test_t) :: test
      end function rhyme_assertion_to_be_ir8

      pure module function rhyme_assertion_to_be_ri(val, exp) result(test)
         real(kind=4), intent(in) :: val
         integer, intent(in) :: exp
         type(test_t) :: test
      end function rhyme_assertion_to_be_ri

      pure module function rhyme_assertion_to_be_rr(val, exp) result(test)
         real(kind=4), intent(in) :: val, exp
         type(test_t) :: test
      end function rhyme_assertion_to_be_rr

      pure module function rhyme_assertion_to_be_rr8(val, exp) result(test)
         real(kind=4), intent(in) :: val
         real(kind=8), intent(in) :: exp
         type(test_t) :: test
      end function rhyme_assertion_to_be_rr8

      pure module function rhyme_assertion_to_be_r8i(val, exp) result(test)
         real(kind=8), intent(in) :: val
         integer, intent(in) :: exp
         type(test_t) :: test
      end function rhyme_assertion_to_be_r8i

      pure module function rhyme_assertion_to_be_r8r(val, exp) result(test)
         real(kind=8), intent(in) :: val
         real(kind=4), intent(in) :: exp
         type(test_t) :: test
      end function rhyme_assertion_to_be_r8r

      pure module function rhyme_assertion_to_be_r8r8(val, exp) result(test)
         real(kind=8), intent(in) :: val, exp
         type(test_t) :: test
      end function rhyme_assertion_to_be_r8r8

      pure module function rhyme_assertion_to_be_chch(val, exp) result(test)
         character(len=*), intent(in) :: val, exp
         type(test_t) :: test
      end function rhyme_assertion_to_be_chch

      pure module function rhyme_assertion_to_be_ll(val, exp) result(test)
         logical, intent(in) :: val, exp
         type(test_t) :: test
      end function rhyme_assertion_to_be_ll

      pure module function rhyme_assertion_not_to_be_ii(val, exp) result(test)
         integer, intent(in) :: val, exp
         type(test_t) :: test
      end function rhyme_assertion_not_to_be_ii

      pure module function rhyme_assertion_not_to_be_ir(val, exp) result(test)
         integer, intent(in) :: val
         real(kind=4), intent(in) :: exp
         type(test_t) :: test
      end function rhyme_assertion_not_to_be_ir

      pure module function rhyme_assertion_not_to_be_ir8(val, exp) result(test)
         integer, intent(in) :: val
         real(kind=8), intent(in) :: exp
         type(test_t) :: test
      end function rhyme_assertion_not_to_be_ir8

      pure module function rhyme_assertion_not_to_be_ri(val, exp) result(test)
         real(kind=4), intent(in) :: val
         integer, intent(in) :: exp
         type(test_t) :: test
      end function rhyme_assertion_not_to_be_ri

      pure module function rhyme_assertion_not_to_be_rr(val, exp) result(test)
         real(kind=4), intent(in) :: val, exp
         type(test_t) :: test
      end function rhyme_assertion_not_to_be_rr

      pure module function rhyme_assertion_not_to_be_rr8(val, exp) result(test)
         real(kind=4), intent(in) :: val
         real(kind=8), intent(in) :: exp
         type(test_t) :: test
      end function rhyme_assertion_not_to_be_rr8

      pure module function rhyme_assertion_not_to_be_r8i(val, exp) result(test)
         real(kind=8), intent(in) :: val
         integer, intent(in) :: exp
         type(test_t) :: test
      end function rhyme_assertion_not_to_be_r8i

      pure module function rhyme_assertion_not_to_be_r8r(val, exp) result(test)
         real(kind=8), intent(in) :: val
         real(kind=4), intent(in) :: exp
         type(test_t) :: test
      end function rhyme_assertion_not_to_be_r8r

      pure module function rhyme_assertion_not_to_be_r8r8(val, exp) result(test)
         real(kind=8), intent(in) :: val, exp
         type(test_t) :: test
      end function rhyme_assertion_not_to_be_r8r8

      pure module function rhyme_assertion_not_to_be_chch(val, exp) result(test)
         character(len=*), intent(in) :: val, exp
         type(test_t) :: test
      end function rhyme_assertion_not_to_be_chch

      pure module function rhyme_assertion_not_to_be_ll(val, exp) result(test)
         logical, intent(in) :: val, exp
         type(test_t) :: test
      end function rhyme_assertion_not_to_be_ll

      pure module function rhyme_assertion_to_be_array_ii(arr1, arr2) result(test)
         integer, intent(in) :: arr1(:), arr2(:)
         type(test_t) :: test
      end function rhyme_assertion_to_be_array_ii

      pure module function rhyme_assertion_to_be_array_ir(arr1, arr2) result(test)
         integer, intent(in) :: arr1(:)
         real(kind=4), intent(in) :: arr2(:)
         type(test_t) :: test
      end function rhyme_assertion_to_be_array_ir

      pure module function rhyme_assertion_to_be_array_ir8(arr1, arr2) result(test)
         integer, intent(in) :: arr1(:)
         real(kind=8), intent(in) :: arr2(:)
         type(test_t) :: test
      end function rhyme_assertion_to_be_array_ir8

      pure module function rhyme_assertion_to_be_array_ri(arr1, arr2) result(test)
         real(kind=4), intent(in) :: arr1(:)
         integer, intent(in) :: arr2(:)
         type(test_t) :: test
      end function rhyme_assertion_to_be_array_ri

      pure module function rhyme_assertion_to_be_array_rr(arr1, arr2) result(test)
         real(kind=4), intent(in) :: arr1(:), arr2(:)
         type(test_t) :: test
      end function rhyme_assertion_to_be_array_rr

      pure module function rhyme_assertion_to_be_array_rr8(arr1, arr2) result(test)
         real(kind=4), intent(in) :: arr1(:)
         real(kind=8), intent(in) :: arr2(:)
         type(test_t) :: test
      end function rhyme_assertion_to_be_array_rr8

      pure module function rhyme_assertion_to_be_array_r8i(arr1, arr2) result(test)
         real(kind=8), intent(in) :: arr1(:)
         integer, intent(in) :: arr2(:)
         type(test_t) :: test
      end function rhyme_assertion_to_be_array_r8i

      pure module function rhyme_assertion_to_be_array_r8r(arr1, arr2) result(test)
         real(kind=8), intent(in) :: arr1(:)
         real(kind=4), intent(in) :: arr2(:)
         type(test_t) :: test
      end function rhyme_assertion_to_be_array_r8r

      pure module function rhyme_assertion_to_be_array_r8r8(arr1, arr2) result(test)
         real(kind=8), intent(in) :: arr1(:), arr2(:)
         type(test_t) :: test
      end function rhyme_assertion_to_be_array_r8r8

      pure module function rhyme_assertion_to_be_array_chch(arr1, arr2) result(test)
         character(len=*), intent(in) :: arr1(:), arr2(:)
         type(test_t) :: test
      end function rhyme_assertion_to_be_array_chch

      pure module function rhyme_assertion_to_be_array_ll(arr1, arr2) result(test)
         logical, intent(in) :: arr1(:), arr2(:)
         type(test_t) :: test
      end function rhyme_assertion_to_be_array_ll

      pure module function rhyme_assertion_not_to_be_array_ii(arr1, arr2) result(test)
         integer, intent(in) :: arr1(:), arr2(:)
         type(test_t) :: test
      end function rhyme_assertion_not_to_be_array_ii

      pure module function rhyme_assertion_not_to_be_array_ir(arr1, arr2) result(test)
         integer, intent(in) :: arr1(:)
         real(kind=4), intent(in) :: arr2(:)
         type(test_t) :: test
      end function rhyme_assertion_not_to_be_array_ir

      pure module function rhyme_assertion_not_to_be_array_ir8(arr1, arr2) result(test)
         integer, intent(in) :: arr1(:)
         real(kind=8), intent(in) :: arr2(:)
         type(test_t) :: test
      end function rhyme_assertion_not_to_be_array_ir8

      pure module function rhyme_assertion_not_to_be_array_ri(arr1, arr2) result(test)
         real(kind=4), intent(in) :: arr1(:)
         integer, intent(in) :: arr2(:)
         type(test_t) :: test
      end function rhyme_assertion_not_to_be_array_ri

      pure module function rhyme_assertion_not_to_be_array_rr(arr1, arr2) result(test)
         real(kind=4), intent(in) :: arr1(:), arr2(:)
         type(test_t) :: test
      end function rhyme_assertion_not_to_be_array_rr

      pure module function rhyme_assertion_not_to_be_array_rr8(arr1, arr2) result(test)
         real(kind=4), intent(in) :: arr1(:)
         real(kind=8), intent(in) :: arr2(:)
         type(test_t) :: test
      end function rhyme_assertion_not_to_be_array_rr8

      pure module function rhyme_assertion_not_to_be_array_r8i(arr1, arr2) result(test)
         real(kind=8), intent(in) :: arr1(:)
         integer, intent(in) :: arr2(:)
         type(test_t) :: test
      end function rhyme_assertion_not_to_be_array_r8i

      pure module function rhyme_assertion_not_to_be_array_r8r(arr1, arr2) result(test)
         real(kind=8), intent(in) :: arr1(:)
         real(kind=4), intent(in) :: arr2(:)
         type(test_t) :: test
      end function rhyme_assertion_not_to_be_array_r8r

      pure module function rhyme_assertion_not_to_be_array_r8r8(arr1, arr2) result(test)
         real(kind=8), intent(in) :: arr1(:), arr2(:)
         type(test_t) :: test
      end function rhyme_assertion_not_to_be_array_r8r8

      pure module function rhyme_assertion_not_to_be_array_chch(arr1, arr2) result(test)
         character(len=*), intent(in) :: arr1(:), arr2(:)
         type(test_t) :: test
      end function rhyme_assertion_not_to_be_array_chch

      pure module function rhyme_assertion_not_to_be_array_ll(arr1, arr2) result(test)
         logical, intent(in) :: arr1(:), arr2(:)
         type(test_t) :: test
      end function rhyme_assertion_not_to_be_array_ll

      pure module function rhyme_assertion_to_be_array_scalar_ii(arr, scalar) result(test)
         integer, intent(in) :: arr(:), scalar
         type(test_t) :: test
      end function rhyme_assertion_to_be_array_scalar_ii

      pure module function rhyme_assertion_to_be_array_scalar_ir(arr, scalar) result(test)
         integer, intent(in) :: arr(:)
         real(kind=4), intent(in) :: scalar
         type(test_t) :: test
      end function rhyme_assertion_to_be_array_scalar_ir

      pure module function rhyme_assertion_to_be_array_scalar_ir8(arr, scalar) result(test)
         integer, intent(in) :: arr(:)
         real(kind=8), intent(in) :: scalar
         type(test_t) :: test
      end function rhyme_assertion_to_be_array_scalar_ir8

      pure module function rhyme_assertion_to_be_array_scalar_ri(arr, scalar) result(test)
         real(kind=4), intent(in) :: arr(:)
         integer, intent(in) :: scalar
         type(test_t) :: test
      end function rhyme_assertion_to_be_array_scalar_ri

      pure module function rhyme_assertion_to_be_array_scalar_rr(arr, scalar) result(test)
         real(kind=4), intent(in) :: arr(:), scalar
         type(test_t) :: test
      end function rhyme_assertion_to_be_array_scalar_rr

      pure module function rhyme_assertion_to_be_array_scalar_rr8(arr, scalar) result(test)
         real(kind=4), intent(in) :: arr(:)
         real(kind=8), intent(in) :: scalar
         type(test_t) :: test
      end function rhyme_assertion_to_be_array_scalar_rr8

      pure module function rhyme_assertion_to_be_array_scalar_r8i(arr, scalar) result(test)
         real(kind=8), intent(in) :: arr(:)
         integer, intent(in) :: scalar
         type(test_t) :: test
      end function rhyme_assertion_to_be_array_scalar_r8i

      pure module function rhyme_assertion_to_be_array_scalar_r8r(arr, scalar) result(test)
         real(kind=8), intent(in) :: arr(:)
         real(kind=4), intent(in) :: scalar
         type(test_t) :: test
      end function rhyme_assertion_to_be_array_scalar_r8r

      pure module function rhyme_assertion_to_be_array_scalar_r8r8(arr, scalar) result(test)
         real(kind=8), intent(in) :: arr(:), scalar
         type(test_t) :: test
      end function rhyme_assertion_to_be_array_scalar_r8r8

      pure module function rhyme_assertion_to_be_array_scalar_chch(arr, scalar) result(test)
         character(len=*), intent(in) :: arr(:), scalar
         type(test_t) :: test
      end function rhyme_assertion_to_be_array_scalar_chch

      pure module function rhyme_assertion_to_be_array_scalar_ll(arr, scalar) result(test)
         logical, intent(in) :: arr(:), scalar
         type(test_t) :: test
      end function rhyme_assertion_to_be_array_scalar_ll

      pure module function rhyme_assertion_not_to_be_array_scalar_ii(arr, scalar) result(test)
         integer, intent(in) :: arr(:), scalar
         type(test_t) :: test
      end function rhyme_assertion_not_to_be_array_scalar_ii

      pure module function rhyme_assertion_not_to_be_array_scalar_ir(arr, scalar) result(test)
         integer, intent(in) :: arr(:)
         real(kind=4), intent(in) :: scalar
         type(test_t) :: test
      end function rhyme_assertion_not_to_be_array_scalar_ir

      pure module function rhyme_assertion_not_to_be_array_scalar_ir8(arr, scalar) result(test)
         integer, intent(in) :: arr(:)
         real(kind=8), intent(in) :: scalar
         type(test_t) :: test
      end function rhyme_assertion_not_to_be_array_scalar_ir8

      pure module function rhyme_assertion_not_to_be_array_scalar_ri(arr, scalar) result(test)
         real(kind=4), intent(in) :: arr(:)
         integer, intent(in) :: scalar
         type(test_t) :: test
      end function rhyme_assertion_not_to_be_array_scalar_ri

      pure module function rhyme_assertion_not_to_be_array_scalar_rr(arr, scalar) result(test)
         real(kind=4), intent(in) :: arr(:), scalar
         type(test_t) :: test
      end function rhyme_assertion_not_to_be_array_scalar_rr

      pure module function rhyme_assertion_not_to_be_array_scalar_rr8(arr, scalar) result(test)
         real(kind=4), intent(in) :: arr(:)
         real(kind=8), intent(in) :: scalar
         type(test_t) :: test
      end function rhyme_assertion_not_to_be_array_scalar_rr8

      pure module function rhyme_assertion_not_to_be_array_scalar_r8i(arr, scalar) result(test)
         real(kind=8), intent(in) :: arr(:)
         integer, intent(in) :: scalar
         type(test_t) :: test
      end function rhyme_assertion_not_to_be_array_scalar_r8i

      pure module function rhyme_assertion_not_to_be_array_scalar_r8r(arr, scalar) result(test)
         real(kind=8), intent(in) :: arr(:)
         real(kind=4), intent(in) :: scalar
         type(test_t) :: test
      end function rhyme_assertion_not_to_be_array_scalar_r8r

      pure module function rhyme_assertion_not_to_be_array_scalar_r8r8(arr, scalar) result(test)
         real(kind=8), intent(in) :: arr(:), scalar
         type(test_t) :: test
      end function rhyme_assertion_not_to_be_array_scalar_r8r8

      pure module function rhyme_assertion_not_to_be_array_scalar_chch(arr, scalar) result(test)
         character(len=*), intent(in) :: arr(:), scalar
         type(test_t) :: test
      end function rhyme_assertion_not_to_be_array_scalar_chch

      pure module function rhyme_assertion_not_to_be_array_scalar_ll(arr, scalar) result(test)
         logical, intent(in) :: arr(:), scalar
         type(test_t) :: test
      end function rhyme_assertion_not_to_be_array_scalar_ll

      pure module function rhyme_assertion_to_be_array_2d_ii(arr1, arr2) result(test)
         integer, intent(in) :: arr1(:, :), arr2(:, :)
         type(test_t) :: test
      end function rhyme_assertion_to_be_array_2d_ii

      pure module function rhyme_assertion_to_be_array_2d_ir(arr1, arr2) result(test)
         integer, intent(in) :: arr1(:, :)
         real(kind=4), intent(in) :: arr2(:, :)
         type(test_t) :: test
      end function rhyme_assertion_to_be_array_2d_ir

      pure module function rhyme_assertion_to_be_array_2d_ir8(arr1, arr2) result(test)
         integer, intent(in) :: arr1(:, :)
         real(kind=8), intent(in) :: arr2(:, :)
         type(test_t) :: test
      end function rhyme_assertion_to_be_array_2d_ir8

      pure module function rhyme_assertion_to_be_array_2d_ri(arr1, arr2) result(test)
         real(kind=4), intent(in) :: arr1(:, :)
         integer, intent(in) :: arr2(:, :)
         type(test_t) :: test
      end function rhyme_assertion_to_be_array_2d_ri

      pure module function rhyme_assertion_to_be_array_2d_rr(arr1, arr2) result(test)
         real(kind=4), intent(in) :: arr1(:, :), arr2(:, :)
         type(test_t) :: test
      end function rhyme_assertion_to_be_array_2d_rr

      pure module function rhyme_assertion_to_be_array_2d_rr8(arr1, arr2) result(test)
         real(kind=4), intent(in) :: arr1(:, :)
         real(kind=8), intent(in) :: arr2(:, :)
         type(test_t) :: test
      end function rhyme_assertion_to_be_array_2d_rr8

      pure module function rhyme_assertion_to_be_array_2d_r8i(arr1, arr2) result(test)
         real(kind=8), intent(in) :: arr1(:, :)
         integer, intent(in) :: arr2(:, :)
         type(test_t) :: test
      end function rhyme_assertion_to_be_array_2d_r8i

      pure module function rhyme_assertion_to_be_array_2d_r8r(arr1, arr2) result(test)
         real(kind=8), intent(in) :: arr1(:, :)
         real(kind=4), intent(in) :: arr2(:, :)
         type(test_t) :: test
      end function rhyme_assertion_to_be_array_2d_r8r

      pure module function rhyme_assertion_to_be_array_2d_r8r8(arr1, arr2) result(test)
         real(kind=8), intent(in) :: arr1(:, :), arr2(:, :)
         type(test_t) :: test
      end function rhyme_assertion_to_be_array_2d_r8r8

      pure module function rhyme_assertion_to_be_array_2d_chch(arr1, arr2) result(test)
         character(len=*), intent(in) :: arr1(:, :), arr2(:, :)
         type(test_t) :: test
      end function rhyme_assertion_to_be_array_2d_chch

      pure module function rhyme_assertion_to_be_array_2d_ll(arr1, arr2) result(test)
         logical, intent(in) :: arr1(:, :), arr2(:, :)
         type(test_t) :: test
      end function rhyme_assertion_to_be_array_2d_ll

      pure module function rhyme_assertion_to_be_array_2d_scalar_ii(arr, scalar) result(test)
         integer, intent(in) :: arr(:, :), scalar
         type(test_t) :: test
      end function rhyme_assertion_to_be_array_2d_scalar_ii

      pure module function rhyme_assertion_to_be_array_2d_scalar_ir(arr, scalar) result(test)
         integer, intent(in) :: arr(:, :)
         real(kind=4), intent(in) :: scalar
         type(test_t) :: test
      end function rhyme_assertion_to_be_array_2d_scalar_ir

      pure module function rhyme_assertion_to_be_array_2d_scalar_ir8(arr, scalar) result(test)
         integer, intent(in) :: arr(:, :)
         real(kind=8), intent(in) :: scalar
         type(test_t) :: test
      end function rhyme_assertion_to_be_array_2d_scalar_ir8

      pure module function rhyme_assertion_to_be_array_2d_scalar_ri(arr, scalar) result(test)
         real(kind=4), intent(in) :: arr(:, :)
         integer, intent(in) :: scalar
         type(test_t) :: test
      end function rhyme_assertion_to_be_array_2d_scalar_ri

      pure module function rhyme_assertion_to_be_array_2d_scalar_rr(arr, scalar) result(test)
         real(kind=4), intent(in) :: arr(:, :), scalar
         type(test_t) :: test
      end function rhyme_assertion_to_be_array_2d_scalar_rr

      pure module function rhyme_assertion_to_be_array_2d_scalar_rr8(arr, scalar) result(test)
         real(kind=4), intent(in) :: arr(:, :)
         real(kind=8), intent(in) :: scalar
         type(test_t) :: test
      end function rhyme_assertion_to_be_array_2d_scalar_rr8

      pure module function rhyme_assertion_to_be_array_2d_scalar_r8i(arr, scalar) result(test)
         real(kind=8), intent(in) :: arr(:, :)
         integer, intent(in) :: scalar
         type(test_t) :: test
      end function rhyme_assertion_to_be_array_2d_scalar_r8i

      pure module function rhyme_assertion_to_be_array_2d_scalar_r8r(arr, scalar) result(test)
         real(kind=8), intent(in) :: arr(:, :)
         real(kind=4), intent(in) :: scalar
         type(test_t) :: test
      end function rhyme_assertion_to_be_array_2d_scalar_r8r

      pure module function rhyme_assertion_to_be_array_2d_scalar_r8r8(arr, scalar) result(test)
         real(kind=8), intent(in) :: arr(:, :), scalar
         type(test_t) :: test
      end function rhyme_assertion_to_be_array_2d_scalar_r8r8

      pure module function rhyme_assertion_to_be_array_3d_ii(arr1, arr2) result(test)
         integer, intent(in) :: arr1(:, :, :), arr2(:, :, :)
         type(test_t) :: test
      end function rhyme_assertion_to_be_array_3d_ii

      pure module function rhyme_assertion_to_be_array_3d_ir(arr1, arr2) result(test)
         integer, intent(in) :: arr1(:, :, :)
         real(kind=4), intent(in) :: arr2(:, :, :)
         type(test_t) :: test
      end function rhyme_assertion_to_be_array_3d_ir

      pure module function rhyme_assertion_to_be_array_3d_ir8(arr1, arr2) result(test)
         integer, intent(in) :: arr1(:, :, :)
         real(kind=8), intent(in) :: arr2(:, :, :)
         type(test_t) :: test
      end function rhyme_assertion_to_be_array_3d_ir8

      pure module function rhyme_assertion_to_be_array_3d_ri(arr1, arr2) result(test)
         real(kind=4), intent(in) :: arr1(:, :, :)
         integer, intent(in) :: arr2(:, :, :)
         type(test_t) :: test
      end function rhyme_assertion_to_be_array_3d_ri

      pure module function rhyme_assertion_to_be_array_3d_rr(arr1, arr2) result(test)
         real(kind=4), intent(in) :: arr1(:, :, :), arr2(:, :, :)
         type(test_t) :: test
      end function rhyme_assertion_to_be_array_3d_rr

      pure module function rhyme_assertion_to_be_array_3d_rr8(arr1, arr2) result(test)
         real(kind=4), intent(in) :: arr1(:, :, :)
         real(kind=8), intent(in) :: arr2(:, :, :)
         type(test_t) :: test
      end function rhyme_assertion_to_be_array_3d_rr8

      pure module function rhyme_assertion_to_be_array_3d_r8i(arr1, arr2) result(test)
         real(kind=8), intent(in) :: arr1(:, :, :)
         integer, intent(in) :: arr2(:, :, :)
         type(test_t) :: test
      end function rhyme_assertion_to_be_array_3d_r8i

      pure module function rhyme_assertion_to_be_array_3d_r8r(arr1, arr2) result(test)
         real(kind=8), intent(in) :: arr1(:, :, :)
         real(kind=4), intent(in) :: arr2(:, :, :)
         type(test_t) :: test
      end function rhyme_assertion_to_be_array_3d_r8r

      pure module function rhyme_assertion_to_be_array_3d_r8r8(arr1, arr2) result(test)
         real(kind=8), intent(in) :: arr1(:, :, :), arr2(:, :, :)
         type(test_t) :: test
      end function rhyme_assertion_to_be_array_3d_r8r8

      pure module function rhyme_assertion_to_be_array_3d_chch(arr1, arr2) result(test)
         character(len=*), intent(in) :: arr1(:, :, :), arr2(:, :, :)
         type(test_t) :: test
      end function rhyme_assertion_to_be_array_3d_chch

      pure module function rhyme_assertion_to_be_array_3d_ll(arr1, arr2) result(test)
         logical, intent(in) :: arr1(:, :, :), arr2(:, :, :)
         type(test_t) :: test
      end function rhyme_assertion_to_be_array_3d_ll

      pure module function rhyme_assertion_to_be_array_3d_scalar_ii(arr, scalar) result(test)
         integer, intent(in) :: arr(:, :, :), scalar
         type(test_t) :: test
      end function rhyme_assertion_to_be_array_3d_scalar_ii

      pure module function rhyme_assertion_to_be_array_3d_scalar_ir(arr, scalar) result(test)
         integer, intent(in) :: arr(:, :, :)
         real(kind=4), intent(in) :: scalar
         type(test_t) :: test
      end function rhyme_assertion_to_be_array_3d_scalar_ir

      pure module function rhyme_assertion_to_be_array_3d_scalar_ir8(arr, scalar) result(test)
         integer, intent(in) :: arr(:, :, :)
         real(kind=8), intent(in) :: scalar
         type(test_t) :: test
      end function rhyme_assertion_to_be_array_3d_scalar_ir8

      pure module function rhyme_assertion_to_be_array_3d_scalar_ri(arr, scalar) result(test)
         real(kind=4), intent(in) :: arr(:, :, :)
         integer, intent(in) :: scalar
         type(test_t) :: test
      end function rhyme_assertion_to_be_array_3d_scalar_ri

      pure module function rhyme_assertion_to_be_array_3d_scalar_rr(arr, scalar) result(test)
         real(kind=4), intent(in) :: arr(:, :, :), scalar
         type(test_t) :: test
      end function rhyme_assertion_to_be_array_3d_scalar_rr

      pure module function rhyme_assertion_to_be_array_3d_scalar_rr8(arr, scalar) result(test)
         real(kind=4), intent(in) :: arr(:, :, :)
         real(kind=8), intent(in) :: scalar
         type(test_t) :: test
      end function rhyme_assertion_to_be_array_3d_scalar_rr8

      pure module function rhyme_assertion_to_be_array_3d_scalar_r8i(arr, scalar) result(test)
         real(kind=8), intent(in) :: arr(:, :, :)
         integer, intent(in) :: scalar
         type(test_t) :: test
      end function rhyme_assertion_to_be_array_3d_scalar_r8i

      pure module function rhyme_assertion_to_be_array_3d_scalar_r8r(arr, scalar) result(test)
         real(kind=8), intent(in) :: arr(:, :, :)
         real(kind=4), intent(in) :: scalar
         type(test_t) :: test
      end function rhyme_assertion_to_be_array_3d_scalar_r8r

      pure module function rhyme_assertion_to_be_array_3d_scalar_r8r8(arr, scalar) result(test)
         real(kind=8), intent(in) :: arr(:, :, :), scalar
         type(test_t) :: test
      end function rhyme_assertion_to_be_array_3d_scalar_r8r8

      pure module function rhyme_assertion_to_be_array_3d_scalar_chch(arr, scalar) result(test)
         character(len=*), intent(in) :: arr(:, :, :), scalar
         type(test_t) :: test
      end function rhyme_assertion_to_be_array_3d_scalar_chch

      pure module function rhyme_assertion_to_be_array_3d_scalar_ll(arr, scalar) result(test)
         logical, intent(in) :: arr(:, :, :), scalar
         type(test_t) :: test
      end function rhyme_assertion_to_be_array_3d_scalar_ll
   end interface

   interface operator(.describe.)
      procedure rhyme_assertion_describe
   end interface operator(.describe.)

   interface operator(.toBe.)
      procedure rhyme_assertion_to_be_ii
      procedure rhyme_assertion_to_be_ir
      procedure rhyme_assertion_to_be_ir8
      procedure rhyme_assertion_to_be_ri
      procedure rhyme_assertion_to_be_rr
      procedure rhyme_assertion_to_be_rr8
      procedure rhyme_assertion_to_be_r8i
      procedure rhyme_assertion_to_be_r8r
      procedure rhyme_assertion_to_be_r8r8
      procedure rhyme_assertion_to_be_chch
      procedure rhyme_assertion_to_be_ll

      procedure rhyme_assertion_to_be_array_ii
      procedure rhyme_assertion_to_be_array_ir
      procedure rhyme_assertion_to_be_array_ir8
      procedure rhyme_assertion_to_be_array_ri
      procedure rhyme_assertion_to_be_array_rr
      procedure rhyme_assertion_to_be_array_rr8
      procedure rhyme_assertion_to_be_array_r8i
      procedure rhyme_assertion_to_be_array_r8r
      procedure rhyme_assertion_to_be_array_r8r8
      procedure rhyme_assertion_to_be_array_chch
      procedure rhyme_assertion_to_be_array_ll

      procedure rhyme_assertion_to_be_array_scalar_ii
      procedure rhyme_assertion_to_be_array_scalar_ir
      procedure rhyme_assertion_to_be_array_scalar_ir8
      procedure rhyme_assertion_to_be_array_scalar_ri
      procedure rhyme_assertion_to_be_array_scalar_rr
      procedure rhyme_assertion_to_be_array_scalar_rr8
      procedure rhyme_assertion_to_be_array_scalar_r8i
      procedure rhyme_assertion_to_be_array_scalar_r8r
      procedure rhyme_assertion_to_be_array_scalar_r8r8
      procedure rhyme_assertion_to_be_array_scalar_chch
      procedure rhyme_assertion_to_be_array_scalar_ll

      procedure rhyme_assertion_to_be_array_2d_ii
      procedure rhyme_assertion_to_be_array_2d_ir
      procedure rhyme_assertion_to_be_array_2d_ir8
      procedure rhyme_assertion_to_be_array_2d_ri
      procedure rhyme_assertion_to_be_array_2d_rr
      procedure rhyme_assertion_to_be_array_2d_rr8
      procedure rhyme_assertion_to_be_array_2d_r8i
      procedure rhyme_assertion_to_be_array_2d_r8r
      procedure rhyme_assertion_to_be_array_2d_r8r8
      procedure rhyme_assertion_to_be_array_2d_chch
      procedure rhyme_assertion_to_be_array_2d_ll

      procedure rhyme_assertion_to_be_array_2d_scalar_ii
      procedure rhyme_assertion_to_be_array_2d_scalar_ir
      procedure rhyme_assertion_to_be_array_2d_scalar_ir8
      procedure rhyme_assertion_to_be_array_2d_scalar_ri
      procedure rhyme_assertion_to_be_array_2d_scalar_rr
      procedure rhyme_assertion_to_be_array_2d_scalar_rr8
      procedure rhyme_assertion_to_be_array_2d_scalar_r8i
      procedure rhyme_assertion_to_be_array_2d_scalar_r8r
      procedure rhyme_assertion_to_be_array_2d_scalar_r8r8

      procedure rhyme_assertion_to_be_array_3d_ii
      procedure rhyme_assertion_to_be_array_3d_ir
      procedure rhyme_assertion_to_be_array_3d_ir8
      procedure rhyme_assertion_to_be_array_3d_ri
      procedure rhyme_assertion_to_be_array_3d_rr
      procedure rhyme_assertion_to_be_array_3d_rr8
      procedure rhyme_assertion_to_be_array_3d_r8i
      procedure rhyme_assertion_to_be_array_3d_r8r
      procedure rhyme_assertion_to_be_array_3d_r8r8
      procedure rhyme_assertion_to_be_array_3d_chch
      procedure rhyme_assertion_to_be_array_3d_ll

      procedure rhyme_assertion_to_be_array_3d_scalar_ii
      procedure rhyme_assertion_to_be_array_3d_scalar_ir
      procedure rhyme_assertion_to_be_array_3d_scalar_ir8
      procedure rhyme_assertion_to_be_array_3d_scalar_ri
      procedure rhyme_assertion_to_be_array_3d_scalar_rr
      procedure rhyme_assertion_to_be_array_3d_scalar_rr8
      procedure rhyme_assertion_to_be_array_3d_scalar_r8i
      procedure rhyme_assertion_to_be_array_3d_scalar_r8r
      procedure rhyme_assertion_to_be_array_3d_scalar_r8r8
      procedure rhyme_assertion_to_be_array_3d_scalar_chch
      procedure rhyme_assertion_to_be_array_3d_scalar_ll
   end interface operator(.toBe.)

   interface operator(.notToBe.)
      procedure rhyme_assertion_not_to_be_ii
      procedure rhyme_assertion_not_to_be_ir
      procedure rhyme_assertion_not_to_be_ir8
      procedure rhyme_assertion_not_to_be_ri
      procedure rhyme_assertion_not_to_be_rr
      procedure rhyme_assertion_not_to_be_rr8
      procedure rhyme_assertion_not_to_be_r8i
      procedure rhyme_assertion_not_to_be_r8r
      procedure rhyme_assertion_not_to_be_r8r8
      procedure rhyme_assertion_not_to_be_chch
      procedure rhyme_assertion_not_to_be_ll

      procedure rhyme_assertion_not_to_be_array_ii
      procedure rhyme_assertion_not_to_be_array_ir
      procedure rhyme_assertion_not_to_be_array_ir8
      procedure rhyme_assertion_not_to_be_array_ri
      procedure rhyme_assertion_not_to_be_array_rr
      procedure rhyme_assertion_not_to_be_array_rr8
      procedure rhyme_assertion_not_to_be_array_r8i
      procedure rhyme_assertion_not_to_be_array_r8r
      procedure rhyme_assertion_not_to_be_array_r8r8
      procedure rhyme_assertion_not_to_be_array_chch
      procedure rhyme_assertion_not_to_be_array_ll

      procedure rhyme_assertion_not_to_be_array_scalar_ii
      procedure rhyme_assertion_not_to_be_array_scalar_ir
      procedure rhyme_assertion_not_to_be_array_scalar_ir8
      procedure rhyme_assertion_not_to_be_array_scalar_ri
      procedure rhyme_assertion_not_to_be_array_scalar_rr
      procedure rhyme_assertion_not_to_be_array_scalar_rr8
      procedure rhyme_assertion_not_to_be_array_scalar_r8i
      procedure rhyme_assertion_not_to_be_array_scalar_r8r
      procedure rhyme_assertion_not_to_be_array_scalar_r8r8
      procedure rhyme_assertion_not_to_be_array_scalar_chch
      procedure rhyme_assertion_not_to_be_array_scalar_ll
   end interface operator(.notToBe.)

   interface operator(.toBeNaN.)
      procedure rhyme_assertion_to_be_nan
      procedure rhyme_assertion_to_be_nan_array
   end interface operator(.toBeNaN.)

   interface operator(.notToBeNaN.)
      procedure rhyme_assertion_not_to_be_nan
      procedure rhyme_assertion_not_to_be_nan_array
   end interface operator(.notToBeNaN.)

   interface operator(.within.)
      procedure rhyme_assertion_within
   end interface operator(.within.)

   interface operator(.hint.)
      procedure rhyme_assertion_add_test_message
   end interface operator(.hint.)
contains

   pure subroutine rhyme_assertion_test_copy_essentials_to(this, test)
      implicit none

      class(test_t), intent(in) :: this
      type(test_t), intent(out) :: test

      test%type = this%type
      test%is_passed = this%is_passed
      test%msg = this%msg
      test%val = this%val
      test%op = this%op
      test%exp = this%exp
      test%within = this%within
      test%real_accuracy = this%real_accuracy
      test%real_val = this%real_val
      test%real_exp = this%real_exp
   end subroutine rhyme_assertion_test_copy_essentials_to

   pure subroutine rhyme_assertion_test_copy_to(this, test)
      implicit none

      class(test_t), intent(inout) :: this
      type(test_t), intent(inout) :: test

      call this%copy_essentials_to(test)

      test%next => this%next
   end subroutine rhyme_assertion_test_copy_to

   pure subroutine rhyme_assertion_test_set_type(this, input)
      implicit none

      class(test_t), intent(inout) :: this
      class(*), intent(in) :: input

      select type (inp => input)
      type is (integer)
         this%type = assertid%int
      type is (real(kind=4))
         this%type = assertid%real
      type is (real(kind=8))
         this%type = assertid%double
      type is (character(*))
         this%type = assertid%char
      type is (logical)
         this%type = assertid%log
      class default
         this%type = assertid%unknown
      end select
   end subroutine rhyme_assertion_test_set_type

   pure subroutine rhyme_assertion_test_set_real_val(this, input)
      use, intrinsic :: ieee_arithmetic

      implicit none

      class(test_t), intent(inout) :: this
      class(*), intent(in) :: input

      select type (inp => input)
      type is (integer)
         this%real_val = real(inp, kind=8)
      type is (real(kind=4))
         this%real_val = real(inp, kind=8)
      type is (real(kind=8))
         this%real_val = inp
      class default
         this%real_val = ieee_value(this%real_val, ieee_quiet_nan)
      end select
   end subroutine rhyme_assertion_test_set_real_val
end module rhyme_assertion
