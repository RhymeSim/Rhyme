logical function rhyme_chombo_init_test() result(failed)
   use rhyme_chombo_factory
   use rhyme_samr_factory
   use rhyme_logger_factory
   use rhyme_assertion

   implicit none

   type(assertion_t) :: ch_tester

   type(chombo_t) :: ch
   type(samr_t) :: samr
   type(logger_t) :: logger

   logical :: exists

   ch_tester = .describe.'chombo init'

   ch = chombo_factory_generate('empty')
   samr = samr_factory%generate()
   logger = logger_factory_generate('default')

   ch%prefix = 'non-existing-directory'

   call rhyme_chombo_init(ch, samr, logger)

   inquire (file=trim(ch%prefix)//'/.', exist=exists)
   call ch_tester%expect(exists.toBe..true.)

   failed = ch_tester%failed()
end function rhyme_chombo_init_test
