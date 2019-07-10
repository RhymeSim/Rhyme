logical function rhyme_assertion_not_to_be_nan_test () result ( failed )
  use rhyme_assertion
  use, intrinsic :: ieee_arithmetic

  implicit none

  type ( test_t ) :: test
  real ( kind=4 ) :: r4
  real ( kind=8 ) :: r8
  character ( len=8 ) :: c
  logical :: l
  integer :: i

  r4 = 1.23e4
  test = .notToBeNaN. r4

  failed = .not. test%is_passed &
  .or. test%type .ne. assertid%real &
  .or. test%op .ne. 'not_to_be_nan' &
  .or. test%real_exp > epsilon(0.d0) &
  .or. test%real_accuracy > epsilon(0.d0) &
  .or. test%within > epsilon(0.d0) &
  .or. abs( test%real_val - 1.23e4 ) > epsilon(0.e0)
  if ( failed ) return

  r8 = 1.23d4
  test = .notToBeNaN. r8

  failed = .not. test%is_passed &
  .or. test%type .ne. assertid%double &
  .or. abs( test%real_val - 1.23d4 ) > epsilon(0.d0)
  if ( failed ) return

  r4 = ieee_value( r4, ieee_quiet_nan )
  test = .notToBeNaN. r4

  failed = test%is_passed &
  .or. test%type .ne. assertid%real &
  .or. test%val .ne. 'NaN' &
  .or. .not. ieee_is_nan( test%real_val )
  if ( failed ) return

  r8 = ieee_value( r8, ieee_quiet_nan )
  test = .notToBeNaN. r8

  failed = test%is_passed &
  .or. test%type .ne. assertid%double &
  .or. test%val .ne. 'NaN' &
  .or. .not. ieee_is_nan( test%real_val )
  if ( failed ) return

  i = 1234
  test = .notToBeNaN. i

  failed = .not. test%is_passed &
  .or. test%type .ne. assertid%int &
  .or. test%val .ne. '1234'
  if ( failed ) return

  c = '1234'
  test = .notToBeNaN. c

  failed = .not. test%is_passed &
  .or. test%type .ne. assertid%char &
  .or. test%val .ne. "1234"
  if ( failed ) return

  l = .false.
  test = .notToBeNaN. l

  failed = .not. test%is_passed &
  .or. test%type .ne. assertid%log &
  .or. test%val .ne. ".false."
  if ( failed ) return
end function rhyme_assertion_not_to_be_nan_test
