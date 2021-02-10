logical function rhyme_sanity_check_perform_test() result(failed)
   use rhyme_sanity_check_factory
   use rhyme_units_factory
   use rhyme_thermo_base_factory
   use rhyme_samr_factory
   use rhyme_logger_factory
   use rhyme_nombre_assertion
   use rhyme_assertion

   implicit none

   type(assertion_t) :: tester

   type(sanity_check_t) :: sc
   type(units_t) :: units
   type(thermo_base_t) :: thermo
   type(samr_t) :: samr
   type(logger_t) :: logger

   tester = .describe."sanity_check_perform"

   sc = sanity_check_factory_generate('default')
   units = units_factory_generate('SI')
   thermo = thermo_base_factory_generate('diatomic')
   samr = samr_factory%generate(physical=.true.)
   logger = logger_factory_generate('default')

   call rhyme_nombre_init
   call rhyme_units_init(units, logger)
   call rhyme_thermo_base_init(thermo, units, logger)

   call rhyme_sanity_check_init(sc, units, thermo, samr, logger)
   call rhyme_sanity_check_perform(sc, samr, logger)

   failed = tester%failed()
end function rhyme_sanity_check_perform_test
