logical function rhyme_muscl_hancock_init_test() result(failed)
   use rhyme_muscl_hancock_factory
   use rhyme_samr_factory
   use rhyme_logger_factory
   use rhyme_assertion

   implicit none

   type(assertion_t) :: tester

   type(muscl_hancock_t) :: mh
   type(mh_workspace_t) :: mhws
   type(samr_t) :: samr
   type(logger_t) :: logger

   tester = .describe."mh_init"

   samr = samr_factory%generate()
   logger = logger_factory_generate('default')

   call tester%expect(mh%solver_type.toBe.mhid%memory_intensive)

   call rhyme_muscl_hancock_init(mh, samr, mhws, logger)

   failed = tester%failed()
end function rhyme_muscl_hancock_init_test
