logical function rhyme_stabilizer_init_test() result(failed)
   use rhyme_stabilizer_factory
   use rhyme_logger_factory
   use rhyme_assertion

   implicit none

   type(assertion_t) :: tester

   type(stabilizer_t) :: st
   type(logger_t) :: logger

   tester = .describe."stabilizer_init"

   st = stabilizer_factory_generate('default')
   logger = logger_factory_generate('default')

   call rhyme_stabilizer_init(st, logger)

   call tester%expect(.false..toBe..true..hint.'Placeholder test')

   failed = tester%failed()
end function rhyme_stabilizer_init_test
