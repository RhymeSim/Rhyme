logical function rhyme_assertion_is_equal_to_test () result ( failed )
  use rhyme_assertion

  implicit none

  integer, parameter :: int_value = 123

  type ( assertion_t ) :: tester
  integer :: int_test

  tester = .describe. 'tester'

  int_test = int_value
  call tester%expect( int_test .isEqualTo. 123 )

  failed = &
  tester%tests%is_passed .neqv. .true. &
  .or. tester%tests%type .ne. assertid%int
end function rhyme_assertion_is_equal_to_test
