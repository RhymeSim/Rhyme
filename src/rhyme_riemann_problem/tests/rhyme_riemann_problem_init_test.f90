logical function rhyme_riemann_problem_init_test() result(failed)
   use rhyme_riemann_problem_factory
   use rhyme_units_factory
   use rhyme_thermo_base_factory
   use rhyme_logger_factory
   use rhyme_assertion

   implicit none

   type(assertion_t) :: tester

   type(riemann_problem_t) :: rp
   type(units_t) :: units
   type(thermo_base_t) :: thermo
   type(logger_t) :: logger

   tester = .describe."riemann_problem_init"

   rp = riemann_problem_factory_generate('default')
   units = units_factory_generate('SI')
   logger = logger_factory_generate('default')

   thermo = thermo_base_factory_generate('diatomic')
   call rhyme_thermo_base_init(thermo, units, logger)

   call rhyme_riemann_problem_init(rp, logger)

   failed = tester%failed()
end function rhyme_riemann_problem_init_test
