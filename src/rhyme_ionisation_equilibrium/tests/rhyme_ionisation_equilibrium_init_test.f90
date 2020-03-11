logical function rhyme_ionisation_equilibrium_init_test() result(failed)
   use rhyme_ionisation_equilibrium_factory
   use rhyme_logger_factory
   use rhyme_assertion

   implicit none

   type(assertion_t) :: tester

   type(ionisation_equilibrium_t) :: ie
   type(logger_t) :: logger

   tester = .describe."ionisation_equilibrium_init"

   ie = ie_factory%generate()
   logger = log_factory%generate()

   call rhyme_ionisation_equilibrium_init(ie, logger)

   failed = tester%failed()

   call ie_factory%final
end function rhyme_ionisation_equilibrium_init_test
