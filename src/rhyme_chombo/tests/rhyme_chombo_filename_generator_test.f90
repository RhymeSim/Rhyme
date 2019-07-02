logical function rhyme_chombo_filename_generator_test () result ( failed )
  use rhyme_chombo_factory
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: ch_tester

  type ( chombo_t ) :: chombo, chombo_empty
  character ( len=1024 ) :: filename

  ch_tester = .describe. "chombo filename_generator"

  chombo = ch_factory%generate()

  chombo%prefix = "./prefix"
  chombo%nickname = "nickname"
  chombo%iteration = 12

  call rhyme_chombo_filename_generator( chombo, filename )
  call ch_tester%expect( filename .toBe. './prefix/nickname-00012.chombo.h5' )

  chombo_empty%iteration = 23
  call rhyme_chombo_filename_generator( chombo_empty, filename )
  call ch_tester%expect( filename .toBe. '00023.chombo.h5' )

  failed = ch_tester%failed()
end function rhyme_chombo_filename_generator_test
