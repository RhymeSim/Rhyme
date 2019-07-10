logical function rhyme_assertion_not_to_be_test () result ( failed )
  use rhyme_assertion

  implicit none

  type ( test_t ) :: test_i, test_r, test_d, test_c, test_l
  character ( len=128 ) :: int_str, real_str, double_str, char_str, log_str

  integer, parameter :: int_value = 123
  real( kind=4 ), parameter :: real_value = 1.23e4
  real( kind=8 ), parameter :: double_value = 12.3d4
  character ( len=8 ), parameter :: char_value = 'string'
  logical, parameter :: log_value = .false.

  integer :: int_test
  real( kind=4 ) :: real_test
  real ( kind=8 ) :: double_test
  character ( len=8 ) :: char_test
  logical :: log_test


  int_test = int_value
  test_i = int_test .notToBe. int_value
  write ( int_str, assertcnst%int_fmt ) int_value

  failed = &
  test_i%is_passed &
  .or. test_i%type .ne. assertid%int &
  .or. test_i%val .ne. int_str &
  .or. trim(test_i%op) .ne. 'not_to_be' &
  .or. test_i%exp .ne. int_str
  if ( failed ) return

  real_test = real_value
  test_r = real_test .notToBe. real_value
  write ( real_str, assertcnst%real_fmt ) real_value

  failed = &
  test_r%is_passed &
  .or. test_r%type .ne. assertid%real &
  .or. test_r%val .ne. real_str &
  .or. test_r%op .ne. 'not_to_be' &
  .or. test_r%exp .ne. real_str
  if ( failed ) return

  double_test = double_value
  test_d = double_test .notToBe. double_value
  write ( double_str, assertcnst%double_fmt ) double_value

  failed = &
  test_d%is_passed &
  .or. test_d%type .ne. assertid%double &
  .or. test_d%val .ne. double_str &
  .or. test_d%op .ne. 'not_to_be' &
  .or. test_d%exp .ne. double_str
  if ( failed ) return

  char_test = char_value
  test_c = char_test .notToBe. char_value
  char_str = char_value

  failed = &
  test_c%is_passed &
  .or. test_c%type .ne. assertid%char &
  .or. test_c%val .ne. trim(char_str) &
  .or. test_c%op .ne. 'not_to_be' &
  .or. test_c%exp .ne. trim(char_str)
  if ( failed ) return

  log_test = log_value
  test_l = log_test .notToBe. log_value
  log_str = '.false.'

  failed = &
  test_l%is_passed &
  .or. test_l%type .ne. assertid%log &
  .or. test_l%val .ne. log_str &
  .or. test_l%op .ne. 'not_to_be' &
  .or. test_l%exp .ne. log_str
  if ( failed ) return

end function rhyme_assertion_not_to_be_test
