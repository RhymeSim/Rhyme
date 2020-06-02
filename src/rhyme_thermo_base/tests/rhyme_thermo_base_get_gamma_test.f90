logical function rhyme_thermo_base_get_gamma_test() result(failed)
   use rhyme_thermo_base_factory
   use rhyme_units_factory
   use rhyme_logger_factory
   use rhyme_assertion

   implicit none

   type(assertion_t) :: th_tester

   type(units_t) :: units
   type(thermo_base_t) :: thermo
   type(logger_t) :: logger

   th_tester = .describe."get_gamma"

   call rhyme_nombre_init

   units = units_factory_generate('SI')
   logger = logger_factory_generate('default')

   thermo = thermo_base_factory_generate('monatomic')
   call rhyme_thermo_base_init(thermo, units, logger)
   call th_tester%expect(rhyme_thermo_base_get_gamma() .toBe.ig_gamma(thid%monatomic))
   call th_tester%expect(rhyme_thermo_base_get_gamma() .toBe.get_gamma())

   thermo = thermo_base_factory_generate('diatomic')
   call rhyme_thermo_base_init(thermo, units, logger)
   call th_tester%expect(rhyme_thermo_base_get_gamma() .toBe.ig_gamma(thid%diatomic))
   call th_tester%expect(rhyme_thermo_base_get_gamma() .toBe.get_gamma())

   thermo = thermo_base_factory_generate('polyatomic')
   call rhyme_thermo_base_init(thermo, units, logger)
   call th_tester%expect(rhyme_thermo_base_get_gamma() .toBe.ig_gamma(thid%polyatomic))
   call th_tester%expect(rhyme_thermo_base_get_gamma() .toBe.get_gamma())

   failed = th_tester%failed()
end function rhyme_thermo_base_get_gamma_test
