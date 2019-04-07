logical function rhyme_assertion_describe_test () result ( failed )
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: tester

  tester = .describe. 'a suit of tests'
  failed = trim( tester%desc ) .ne. 'a suit of tests'
  if ( failed ) return
end function rhyme_assertion_describe_test
