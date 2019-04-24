logical function rhyme_assertion_not_to_be_nan_array_test () result ( failed )
  use, intrinsic :: ieee_arithmetic

  use rhyme_assertion

  implicit none

  type ( test_t ) :: test
  real ( kind=4 ) :: r4(6), r4nan
  real ( kind=8 ) :: r8(6), r8nan

  r4nan = ieee_value( r4nan, ieee_quiet_nan )
  r8nan = ieee_value( r8nan, ieee_quiet_nan )

  r4 = 1.23e4
  test = .notToBeNaN. r4

  failed = .not. test%is_passed &
  .or. test%op .ne. 'not_to_be_nan' &
  .or. .not. ieee_is_nan( test%real_exp ) &
  .or. .not. ieee_is_nan( test%real_accuracy ) &
  .or. .not. ieee_is_nan( test%within ) &
  .or. test%exp .ne. 'NaN' &
  .or. test%type .ne. assertid%real
  if ( failed ) return

  r4(1) = r4nan
  test = .notToBeNaN. r4

  failed = test%is_passed
  if ( failed ) return

  r8 = 2.34d5
  test = .notToBeNaN. r8

  failed = .not. test%is_passed
  if ( failed ) return

  r8(3) = r8nan
  test = .notToBeNaN. r8

  failed = test%is_passed
  if ( failed ) return
end function rhyme_assertion_not_to_be_nan_array_test
