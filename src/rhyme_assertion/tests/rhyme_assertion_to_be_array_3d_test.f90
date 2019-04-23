logical function rhyme_assertion_to_be_array_3d_test () result ( failed )
  use rhyme_assertion

  implicit none

  integer :: int_arr_value(2,2,2)
  real ( kind=4 ) :: real_arr_value(2,2,2)
  real ( kind=8 ) :: double_arr_value(2,2,2)
  character ( len=32 ) :: char_arr_value(2,2,2)
  logical :: log_arr_value(2,2,2)

  integer :: int_arr(2,2,2)
  real ( kind=4 ) :: real_arr(2,2,2)
  real ( kind=8 ) :: double_arr(2,2,2)
  character ( len=32 ) :: char_arr(2,2,2)
  logical :: log_arr(2,2,2)

  type ( test_t ) :: test_i, test_r, test_d, test_c, test_l

  int_arr_value = reshape( [ 1, 2, 3, 4, 5, 6, 7, 8 ], [2, 2, 2] )
  real_arr_value = reshape( &
    [ 1.e0, 2.e0, 3.e0, 4.e0, 5.e0, 6.e0, 7.e0, 8.e0 ], [2, 2, 2] )
  double_arr_value = reshape( &
    [ 1.d0, 2.d0, 3.d0, 4.d0, 5.d0, 6.d0, 7.d0, 8.d0 ], [2, 2, 2] )
  char_arr_value = reshape( [ '1', '2', '3', '4', '5', '6', '7', '8' ], [2, 2, 2] )
  log_arr_value = reshape( &
    [ .false., .true., .false., .true., .true., .false., .true., .false. ], [2, 2, 2] )

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
end function rhyme_assertion_to_be_array_3d_test
