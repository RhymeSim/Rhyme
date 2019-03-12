logical function rhyme_chombo_init_with_test () result ( failed )
  use rhyme_chombo
  use rhyme_log

  implicit none

  type ( chombo_t ) :: ch
  type ( log_t ) :: log

  character ( len=1024 ), parameter :: chombo_fac_prefix = "prefix"
  character ( len=1024 ), parameter :: chombo_fac_nickname = "nickname"


  call ch%init_with ( chombo_fac_prefix, chombo_fac_nickname, log )

  failed = &
  ch%prefix .ne. chombo_fac_prefix &
  .or. ch%nickname .ne. chombo_fac_nickname
end function rhyme_chombo_init_with_test
