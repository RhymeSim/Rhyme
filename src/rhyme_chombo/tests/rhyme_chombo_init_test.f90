logical function rhyme_chombo_init_test() result(failed)
   use rhyme_chombo_factory
   use rhyme_samr_factory
   use rhyme_logger_factory
   use rhyme_assertion

   implicit none

   type(assertion_t) :: ch_tester

   type(chombo_t) :: chombo
   type(samr_t) :: samr
   type(logger_t) :: logger

   logical :: exists

   ch_tester = .describe.'chombo init'

   chombo = ch_factory%generate()
   samr = samr_factory%generate()
   logger = log_factory%generate()

   chombo%prefix = 'non-existing-directory'

   call rhyme_chombo_init(chombo, samr, logger)

   inquire (file=trim(chombo%prefix)//'/.', exist=exists)
   call ch_tester%expect(exists.toBe..true.)

   failed = ch_tester%failed()
end function rhyme_chombo_init_test
