logical function rhyme_chombo_init_with_test () result ( failed )
  use rhyme_chombo
  use rhyme_log
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: ch_tester

  type ( chombo_t ) :: ch
  type ( log_t ) :: log

  character ( len=1024 ), parameter :: chombo_fac_prefix = "prefix"
  character ( len=1024 ), parameter :: chombo_fac_nickname = "nickname"

  ch_tester = .describe. "chombo init_with"

  call ch%init_with ( chombo_fac_prefix, chombo_fac_nickname, log )

  call ch_tester%expect( ch%prefix .toBe. chombo_fac_prefix )
  call ch_tester%expect( ch%nickname .toBe. chombo_fac_nickname )

  failed = ch_tester%failed()
end function rhyme_chombo_init_with_test
