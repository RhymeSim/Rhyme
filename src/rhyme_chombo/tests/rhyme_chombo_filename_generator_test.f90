logical function rhyme_chombo_filename_generator_test () result ( failed )
  use rhyme_chombo

  implicit none

  type ( chombo_t ) :: chombo, chombo_empty

  character ( len=1024 ) :: filename

  chombo%prefix = "./prefix"
  chombo%nickname = "nickname"
  chombo%iteration = 12

  call chombo%filename_generator ( filename )
  failed = trim(filename) .ne. "./prefix/nickname-00012.chombo.h5"
  if ( failed ) return

  chombo_empty%iteration = 23
  call chombo_empty%filename_generator ( filename )
  failed = trim(filename) .ne. "00023.chombo.h5"
end function rhyme_chombo_filename_generator_test
