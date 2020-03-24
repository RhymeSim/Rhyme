logical function rhyme_periodic_table_init_test() result(failed)
   use rhyme_periodic_table_factory
   use rhyme_logger_factory
   use rhyme_assertion

   implicit none

   type(assertion_t) :: tester

   type(periodic_table_t) :: pt
   type(logger_t) :: logger

   tester = .describe."periodic_table_init"

   pt = periodic_table_factory%generate()
   logger = log_factory%generate()

   call rhyme_periodic_table_init(pt, logger)

   call tester%expect(.false..toBe..true..hint.'Placeholder test')

   failed = tester%failed()

   call periodic_table_factory%final
end function rhyme_periodic_table_init_test
