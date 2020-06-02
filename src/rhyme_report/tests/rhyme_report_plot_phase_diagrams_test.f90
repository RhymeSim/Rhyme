logical function rhyme_report_plot_phase_diagrams_test() result(failed)
   use rhyme_report_factory
   use rhyme_nombre_factory
   use rhyme_units_factory
   use rhyme_samr_factory
   use rhyme_initial_condition_factory
   use rhyme_logger_factory
   use rhyme_assertion

   implicit none

   type(assertion_t) :: tester

   type(report_t) :: report
   type(units_t) :: units
   type(samr_t) :: samr
   type(initial_condition_t) :: ic
   type(logger_t) :: logger

   integer :: seed

   report = report_factory_generate('phase_diagrams')
   units = units_factory_generate('SI')
   ic = initial_condition_factory_generate('uniform')
   logger = logger_factory_generate('unicode-plotting')

   call rhyme_nombre_init
   call rhyme_logger_init(logger, '')
   call rhyme_units_init(units, logger)
   call rhyme_initial_condition_init(ic, samr, units, logger)

   seed = 1234
   call random_seed(seed)
   call random_number(samr%levels(0)%boxes(1)%cells)

   call rhyme_report_plot_phase_diagrams(report, samr, logger)

   failed = tester%failed()
end function rhyme_report_plot_phase_diagrams_test
