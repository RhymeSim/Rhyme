logical function rhyme_assertion_to_be_array_scalar_test () result ( failed )
  use rhyme_assertion

  implicit none


  integer, parameter :: int_arr_value = 3
  real ( kind=4 ), parameter :: real_arr_value = 3.e0
  real ( kind=8 ), parameter :: double_arr_value = 3.d0
  character ( len=32 ), parameter :: char_arr_value = '3'
  logical, parameter :: log_arr_value = .true.

  integer :: int_arr(5)
  real ( kind=4 ) :: real_arr(5)
  real ( kind=8 ) :: double_arr(5)
  character ( len=32 ) :: char_arr(5)
  logical :: log_arr(5)

  type ( test_t ) :: test_i, test_r, test_d, test_c, test_l

  int_arr = int_arr_value
  test_i = int_arr .toBe. int_arr_value

  failed = &
  .not. test_i%is_passed &
  .or. test_i%type .ne. assertid%int_arr
  if ( failed ) return

  real_arr = real_arr_value
  test_r = real_arr .toBe. real_arr_value

  failed = &
  .not. test_r%is_passed &
  .or. test_r%type .ne. assertid%real_arr
  if ( failed ) return

  double_arr = double_arr_value
  test_d = double_arr .toBe. double_arr_value

  failed = &
  .not. test_d%is_passed &
  .or. test_d%type .ne. assertid%double_arr
  if ( failed ) return

  char_arr = char_arr_value
  test_c = char_arr .toBe. char_arr_value

  failed = &
  .not. test_c%is_passed &
  .or. test_c%type .ne. assertid%char_arr
  if ( failed ) return

  log_arr = log_arr_value
  test_l = log_arr .toBe. log_arr_value

  failed = &
  .not. test_l%is_passed &
  .or. test_l%type .ne. assertid%log_arr
  if ( failed ) return
end function rhyme_assertion_to_be_array_scalar_test
