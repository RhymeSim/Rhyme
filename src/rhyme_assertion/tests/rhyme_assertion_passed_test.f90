logical function rhyme_assertion_passed_test () result ( failed )
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: tester

  tester = .describe. 'rhyme_assertion_passed_test'

  failed = .not. tester%passed()
  if ( failed ) return

  call tester%expect( 1 .toBe. 1 .when. 'test should pass' )
  call tester%expect( 2 .toBe. 2 .when. 'test should pass' )
  call tester%expect( 3 .toBe. 3 .when. 'test should pass' )

  failed = .not. tester%passed()
  if ( failed ) return

  call tester%expect( 4 .toBe. 5 .when. 'test must fail' )
  failed = tester%passed()
  if ( failed ) return
end function rhyme_assertion_passed_test
