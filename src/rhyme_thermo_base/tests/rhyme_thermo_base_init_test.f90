logical function rhyme_thermo_base_init_test() result(failed)
   use rhyme_thermo_base_factory
   use rhyme_units_factory
   use rhyme_logger_factory
   use rhyme_assertion

   implicit none

   type(assertion_t) :: th_tester

   type(units_t) :: units
   type(thermo_base_t) :: thermo
   type(logger_t) :: logger

   th_tester = .describe."init"

   call rhyme_nombre_init

   units = units_factory_generate('SI')
   logger = logger_factory_generate('default')

   thermo = thermo_base_factory_generate('diatomic')

   call rhyme_thermo_base_init(thermo, units, logger)

   failed = th_tester%failed()
end function rhyme_thermo_base_init_test
