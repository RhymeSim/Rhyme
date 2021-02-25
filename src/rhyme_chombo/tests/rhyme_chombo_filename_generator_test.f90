logical function rhyme_chombo_filename_generator_test() result(failed)
   use rhyme_chombo_factory
   use rhyme_assertion

   implicit none

   type(assertion_t) :: ch_tester

   type(chombo_t) :: ch, ch_empty
   character(len=1024) :: filename

   ch_tester = .describe."chombo filename_generator"

   ch = chombo_factory_generate('empty')

   ch%prefix = "./prefix"
   ch%nickname = "nickname"
   ch%iteration = 12

   call rhyme_chombo_filename_generator(ch%prefix, ch%nickname, ch%iteration, filename)
   call ch_tester%expect(filename.toBe.'./prefix/nickname-000012.chombo.h5')

   ch_empty%iteration = 23
   call rhyme_chombo_filename_generator(ch_empty%prefix, ch_empty%nickname, ch_empty%iteration, filename)
   call ch_tester%expect(filename.toBe.'000023.chombo.h5')

   failed = ch_tester%failed()
end function rhyme_chombo_filename_generator_test
