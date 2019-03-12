logical function rhyme_chombo_filename_generator_test () result ( failed )
  use rhyme_chombo

  implicit none

  type ( chombo_t ) :: ch, ch_empty

  character ( len=1024 ) :: filename

  ch%prefix = "./prefix"
  ch%nickname = "nickname"
  ch%iteration = 12

  call ch%filename_generator ( filename )
  failed = trim(filename) .ne. "./prefix/nickname-00012.chombo.h5"
  if ( failed ) return

  ch_empty%iteration = 23
  call ch_empty%filename_generator ( filename )
  failed = trim(filename) .ne. "00023.chombo.h5"
end function rhyme_chombo_filename_generator_test
