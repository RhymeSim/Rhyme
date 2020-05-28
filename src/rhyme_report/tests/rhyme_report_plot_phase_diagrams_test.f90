logical function rhyme_report_plot_phase_diagrams_test() result(failed)
   use rhyme_report_factory
   use rhyme_nombre_factory
   use rhyme_physics_factory
   use rhyme_samr_factory
   use rhyme_initial_condition_factory
   use rhyme_logger_factory
   use rhyme_assertion

   implicit none

   type(assertion_t) :: tester

   type(report_t) :: report
   type(physics_t) :: physics
   type(samr_t) :: samr
   type(initial_condition_t) :: ic
   type(logger_t) :: logger

   report = report_factory_generate('phase_diagrams')
   physics = physics_factory_generate('SI')
   ic = initial_condition_factory_generate('uniform')
   logger = logger_factory_generate('unicode-plotting')

   call rhyme_nombre_init
   call rhyme_logger_init(logger, '')
   call rhyme_physics_init(physics, logger)
   call rhyme_initial_condition_init(ic, samr, physics, logger)

   call random_number(samr%levels(0)%boxes(1)%cells)

   call rhyme_report_plot_phase_diagrams(report, samr, logger)

   failed = tester%failed()
end function rhyme_report_plot_phase_diagrams_test
