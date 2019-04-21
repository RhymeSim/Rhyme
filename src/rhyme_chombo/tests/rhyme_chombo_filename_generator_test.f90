logical function rhyme_chombo_filename_generator_test () result ( failed )
  use rhyme_chombo
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: ch_tester

  type ( chombo_t ) :: ch, ch_empty
  character ( len=1024 ) :: filename

  ch_tester = .describe. "chombo filename_generator"

  ch%prefix = "./prefix"
  ch%nickname = "nickname"
  ch%iteration = 12

  call ch%filename_generator ( filename )
  call ch_tester%expect( filename .toBe. './prefix/nickname-00012.chombo.h5' )

  ch_empty%iteration = 23
  call ch_empty%filename_generator ( filename )
  call ch_tester%expect( filename .toBe. '00023.chombo.h5' )

  failed = ch_tester%failed()
end function rhyme_chombo_filename_generator_test
