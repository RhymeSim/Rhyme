logical function rhyme_stabilizer_init_test() result(failed)
   use rhyme_stabilizer_factory
   use rhyme_samr_factory
   use rhyme_logger_factory
   use rhyme_assertion

   implicit none

   type(assertion_t) :: tester

   type(stabilizer_t) :: st
   type(samr_t) :: samr
   type(logger_t) :: logger

   tester = .describe."stabilizer_init"

   st = stabilizer_factory_generate('default')
   samr = samr_factory%generate(physical=.true.)
   logger = logger_factory_generate('default')

   call rhyme_stabilizer_init(st, samr, logger)

   call tester%expect(st%target_center.notToBe.0d0.hint.'Target center')
   call tester%expect(.notToBeNaN.st%target_center.hint.'Target center (NaN)')

   failed = tester%failed()
end function rhyme_stabilizer_init_test
