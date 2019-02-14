logical function rhyme_chombo_init_with_test () result ( failed )
  use rhyme_chombo

  implicit none

  type ( chombo_t ) :: chombo

  character ( len=1024 ), parameter :: rhyme_chombo_factory_prefix = "prefix"
  character ( len=1024 ), parameter :: rhyme_chombo_factory_nickname = "nickname"


  call chombo%init_with ( rhyme_chombo_factory_prefix, rhyme_chombo_factory_nickname )

  failed = &
  chombo%prefix .ne. rhyme_chombo_factory_prefix &
  .or. chombo%nickname .ne. rhyme_chombo_factory_nickname
end function rhyme_chombo_init_with_test
