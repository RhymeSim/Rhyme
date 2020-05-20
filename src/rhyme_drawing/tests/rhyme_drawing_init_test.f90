logical function rhyme_drawing_init_test() result(failed)
   use rhyme_drawing_factory
   use rhyme_physics_factory
   use rhyme_chemistry_factory
   use rhyme_initial_condition_factory
   use rhyme_ionisation_equilibrium_factory
   use rhyme_uv_background_factory
   use rhyme_logger_factory
   use rhyme_assertion

   implicit none

   type(assertion_t) :: tester

   type(drawing_t) :: draw
   type(physics_t) :: physics
   type(chemistry_t) :: chemistry
   type(samr_t) :: samr
   type(initial_condition_t) :: ic
   type(ionisation_equilibrium_t) :: ie
   type(uv_background_t) :: uvb
   type(logger_t) :: logger

   tester = .describe."drawing_init"

   physics = physics_factory_generate('SI')
   chemistry = chemistry_factory_generate('H+He')
   ic = initial_condition_factory_generate('uniform')
   ie = ionisation_equilibrium_factory_generate('CaseA-CPIE')
   uvb = uv_background_factory_generate('HM12')
   logger = logger_factory_generate('default')

   call rhyme_nombre_init
   call rhyme_physics_init(physics, logger)
   call rhyme_chemistry_init(chemistry, physics, logger)
   call rhyme_initial_condition_init(ic, samr, physics, logger)
   call rhyme_ionisation_equilibrium_init(ie, physics, chemistry, logger)
   call rhyme_uv_background_init(uvb, physics, logger)

   call rhyme_ionisation_equilibrium_update_table(ie, chemistry, uvb, 1.37d0, logger)

   draw%shapes => draw%new_shape(drid%sphere)
   draw%shapes%sphere%unit_str = 'km'
   draw%shapes%next => draw%new_shape(drid%sphere)
   draw%shapes%next%sphere%unit_str = 'pc'

   call rhyme_drawing_init(draw, samr, ic, logger, ie, physics, chemistry)

   failed = tester%failed()
end function rhyme_drawing_init_test
