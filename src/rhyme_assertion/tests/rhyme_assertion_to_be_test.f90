logical function rhyme_assertion_to_be_test () result ( failed )
  use rhyme_assertion

  implicit none

  integer, parameter :: int_value = 123

  type ( test_t ) :: test
  integer :: int_test

  int_test = int_value
  test = int_test .toBe. int_value

  failed = &
  test%is_passed .neqv. .true. &
  .or. test%type .ne. assertid%int
end function rhyme_assertion_to_be_test
