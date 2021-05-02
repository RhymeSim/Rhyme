logical function rhyme_deep_rs_init_test() result(failed)
   use rhyme_deep_rs_factory
   use rhyme_logger_factory
   use rhyme_assertion

   implicit none

   type(assertion_t) :: tester

   type(deep_rs_t) :: drs
   type(logger_t) :: logger

   tester = .describe."deep_rs_init"

   drs = deep_rs_factory_generate('default')
   logger = logger_factory_generate('default')

   call rhyme_deep_rs_init(drs, logger)

   call tester%expect(.false..toBe..true..hint.'Placeholder test')

   failed = tester%failed()
end function rhyme_deep_rs_init_test
