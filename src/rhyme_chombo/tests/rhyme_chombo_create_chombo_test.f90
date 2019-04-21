logical function rhyme_chombo_create_chombo_test () result ( failed )
  use rhyme_chombo
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: ch_tester

  character ( len=1024 ), parameter :: nickname = "rhyme_chombo_create_chombo"
  type ( chombo_t ) :: ch

  ch_tester = .describe. "chombo create"

  ch%nickname = nickname
  call ch%create_chombo

  call ch_tester%expect( ch%is_opened .toBe. .true. )

  failed = ch_tester%failed()
end function rhyme_chombo_create_chombo_test
