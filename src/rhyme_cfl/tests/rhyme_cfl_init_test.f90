logical function rhyme_cfl_init_test() result(failed)
   use rhyme_cfl_factory
   use rhyme_units_factory
   use rhyme_thermo_base_factory
   use rhyme_samr_factory
   use rhyme_logger_factory
   use rhyme_assertion

   implicit none

   type(assertion_t) :: tester

   type(cfl_t) :: cfl
   type(units_t) :: units
   type(thermo_base_t) :: thermo
   type(samr_t) :: samr
   type(logger_t) :: logger

   tester = .describe."CFL"

   call rhyme_nombre_init

   cfl = cfl_factory_generate(2d-1)
   units = units_factory_generate('SI')
   samr = samr_factory%generate(physical=.true.)
   logger = logger_factory_generate('unicode-plotting')

   thermo = thermo_base_factory_generate('diatomic')
   call rhyme_thermo_base_init(thermo, units, logger)

   call rhyme_cfl_init(cfl, thermo, samr, logger)

   ! To see the output, set the following .true.
   failed = .false.
end function rhyme_cfl_init_test
