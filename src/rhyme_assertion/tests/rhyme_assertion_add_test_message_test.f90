logical function rhyme_assertion_add_test_message_test () result ( failed )
  use rhyme_assertion

  implicit none

  type ( test_t ) :: test, test_passed

  test_passed%is_passed = .true.
  test_passed%type = assertid%int
  test_passed%val = 'val'
  test_passed%op = 'op'
  test_passed%exp = 'exp'

  test = test_passed .hint. 'message'

  failed = test%msg .ne. 'message'
end function rhyme_assertion_add_test_message_test
