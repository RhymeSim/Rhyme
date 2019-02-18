logical function rhyme_chombo_create_chombo_test () result ( failed )
  use rhyme_chombo

  implicit none


  character ( len=1024 ), parameter :: nickname = "rhyme_chombo_create_chombo"

  type ( chombo_t ) :: ch


  ch%nickname = nickname
  call ch%create_chombo

  failed = .not. ch%is_opened
end function rhyme_chombo_create_chombo_test
