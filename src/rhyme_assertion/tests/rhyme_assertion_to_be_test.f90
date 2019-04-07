logical function rhyme_assertion_to_be_test () result ( failed )
  use rhyme_assertion

  implicit none

  integer, parameter :: int_value = 123
  real( kind=4 ), parameter :: real_value = 1.23e4
  real( kind=8 ), parameter :: double_value = 12.3d4
  character ( len=8 ), parameter :: char_value = 'string'

  character ( len=128 ) :: int_str, real_str, double_str, char_str

  type ( test_t ) :: test_i, test_r, test_d, test_c
  integer :: int_test
  real( kind=4 ) :: real_test
  real ( kind=8 ) :: double_test
  character ( len=8 ) :: char_test

  int_test = int_value
  test_i = int_test .toBe. int_value
  write ( int_str, assertcnst%int_fmt ) int_value

  failed = &
  .not. test_i%is_passed &
  .or. test_i%type .ne. assertid%int &
  .or. test_i%val .ne. int_str &
  .or. test_i%op .ne. 'to_be' &
  .or. test_i%exp .ne. int_str
  if ( failed ) return

  real_test = real_value
  test_r = real_test .toBe. real_value
  write ( real_str, assertcnst%real_fmt ) real_value

  failed = &
  .not. test_r%is_passed &
  .or. test_r%type .ne. assertid%real &
  .or. test_r%val .ne. real_str &
  .or. test_r%op .ne. 'to_be' &
  .or. test_r%exp .ne. real_str
  if ( failed ) return

  double_test = double_value
  test_d = double_test .toBe. double_value
  write ( double_str, assertcnst%double_fmt ) double_value

  failed = &
  .not. test_d%is_passed &
  .or. test_d%type .ne. assertid%double &
  .or. test_d%val .ne. double_str &
  .or. test_d%op .ne. 'to_be' &
  .or. test_d%exp .ne. double_str
  if ( failed ) return

  char_test = char_value
  test_c = char_test .toBe. char_value
  char_str = char_value

  failed = &
  .not. test_c%is_passed &
  .or. test_c%type .ne. assertid%char &
  .or. test_c%val .ne. char_str &
  .or. test_c%op .ne. 'to_be' &
  .or. test_c%exp .ne. char_str
  if ( failed ) return
end function rhyme_assertion_to_be_test
