logical function rhyme_ionisation_equilibrium_init_test() result(failed)
   use rhyme_ionisation_equilibrium_factory
   use rhyme_logger_factory
   use rhyme_assertion

   implicit none

   type(assertion_t) :: tester

   type(ionisation_equilibrium_t) :: ie
   type(logger_t) :: logger

   tester = .describe."ionisation_equilibrium_init"

   ie = ionisation_equilibrium_factory_generate('default')
   logger = logger_factory_generate('default')

   call rhyme_ionisation_equilibrium_init(ie, logger)

   failed = tester%failed()
end function rhyme_ionisation_equilibrium_init_test
