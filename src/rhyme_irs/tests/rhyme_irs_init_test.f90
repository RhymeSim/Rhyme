logical function rhyme_irs_init_test() result(failed)
   use rhyme_irs_factory
   use rhyme_physics_factory
   use rhyme_thermo_base_factory
   use rhyme_logger_factory
   use rhyme_assertion

   implicit none

   type(assertion_t) :: tester

   type(irs_t) :: irs
   type(physics_t) :: physics
   type(thermo_base_t) :: thermo
   type(logger_t) :: logger

   tester = .describe."irs_init"

   irs = irs_factory_generate('default')
   physics = physics_factory_generate('SI')
   logger = logger_factory_generate('default')

   thermo = thermo_base_factory_generate('diatomic')
   call rhyme_thermo_base_init(thermo, physics, logger)

   call rhyme_irs_init(irs, logger)

   failed = tester%failed()
end function rhyme_irs_init_test
