logical function rhyme_assertion_reset_test () result ( failed )
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: tester

  call tester%expect( 1 .toBe. 1 .hint. 'test should pass' )
  call tester%expect( 2 .toBe. 2 .hint. 'test should pass' )
  call tester%expect( 3 .toBe. 3 .hint. 'test should pass' )

  call tester%reset

  failed = associated( tester%tests )
end function rhyme_assertion_reset_test
