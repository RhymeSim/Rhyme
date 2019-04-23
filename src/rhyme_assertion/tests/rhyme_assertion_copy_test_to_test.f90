logical function rhyme_assertion_copy_test_to_test () result ( failed )
  use rhyme_assertion

  implicit none

  type ( test_t ) :: test, target_test

  test = 1 .toBe. 1 .within. 1e-4

  call test%copy_to( target_test )

  failed = &
  test%type .ne. target_test%type &
  .or. (test%is_passed .neqv. target_test%is_passed) &
  .or. test%msg .ne. target_test%msg &
  .or. test%val .ne. target_test%val &
  .or. test%op .ne. target_test%op &
  .or. test%exp .ne. target_test%exp &
  .or. abs( test%within - target_test%within ) > epsilon(0.d0) &
  .or. abs( test%real_accuracy - target_test%real_accuracy ) > epsilon(0.d0) &
  .or. associated(test%next) .neqv. associated(target_test%next)
end function rhyme_assertion_copy_test_to_test
