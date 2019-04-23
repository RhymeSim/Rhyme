logical function rhyme_assertion_within_test () result ( failed )
  use rhyme_assertion

  implicit none

  type ( test_t ) :: test, res

  test%is_passed = .false.
  test%real_accuracy = 1e-5

  test%type = 1234
  test%msg = 'msg'
  test%val = 'val'
  test%op = 'op'
  test%exp = 'exp'

  res = test .within. 1d-6

  failed = res%is_passed &
  .or. abs( res%within - 1d-6 ) > epsilon(0.d0) &
  .or. res%type .ne. 1234 &
  .or. res%msg .ne. 'msg' &
  .or. res%val .ne. 'val' &
  .or. res%op .ne. 'op' &
  .or. res%exp .ne. 'exp'
  if ( failed ) return

  res = test .within. 1d-4
  failed = .not. res%is_passed
end function rhyme_assertion_within_test
