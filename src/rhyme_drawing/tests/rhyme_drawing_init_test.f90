logical function rhyme_drawing_init_test() result(failed)
   use rhyme_drawing_factory
   use rhyme_physics_factory
   use rhyme_initial_condition_factory
   use rhyme_logger_factory
   use rhyme_assertion

   implicit none

   type(assertion_t) :: tester

   type(drawing_t) :: draw
   type(physics_t) :: physics
   type(samr_t) :: samr
   type(initial_condition_t) :: ic
   type(logger_t) :: logger

   tester = .describe."drawing_init"

   call rhyme_nombre_init

   physics = physics_factory_generate('SI')
   ic = initial_condition_factory_generate('uniform')
   logger = logger_factory_generate('default')

   call rhyme_physics_init(physics, logger)
   call rhyme_initial_condition_init(ic, samr, physics, logger)

   draw%shapes => draw%new_shape(drid%sphere)
   draw%shapes%sphere%unit_str = 'km'
   draw%shapes%next => draw%new_shape(drid%sphere)
   draw%shapes%next%sphere%unit_str = 'pc'

   call rhyme_drawing_init(draw, samr, ic, logger)

   failed = tester%failed()
end function rhyme_drawing_init_test
