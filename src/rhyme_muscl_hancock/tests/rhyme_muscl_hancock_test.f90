logical function rhyme_muscl_hancock_test() result(failed)
   use rhyme_muscl_hancock_factory
   use rhyme_samr_factory
   use rhyme_logger_factory
   use rhyme_assertion

   implicit none

   type(assertion_t) :: mh_tester

   type(muscl_hancock_t) :: mh
   type(mh_workspace_t) :: mhws
   type(samr_t) :: samr
   type(logger_t) :: logger

   mh_tester = .describe."mh"

   samr = samr_factory%generate()
   logger = log_factory%generate()

   call rhyme_muscl_hancock_init(mh, samr, mhws, logger)

   failed = mh_tester%failed()
end function rhyme_muscl_hancock_test
