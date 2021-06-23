logical function rhyme_drawing_init_test() result(failed)
   use rhyme_drawing_factory
   use rhyme_units_factory
   use rhyme_thermo_base_factory
   use rhyme_chemistry_factory
   use rhyme_initial_condition_factory
   use rhyme_ionisation_equilibrium_factory
   use rhyme_uv_background_factory
   use rhyme_logger_factory
   use rhyme_assertion

   implicit none

   type(assertion_t) :: tester

   type(drawing_t) :: draw
   type(units_t) :: units
   type(thermo_base_t) :: thermo
   type(chemistry_t) :: chemistry
   type(samr_t) :: samr
   type(initial_condition_t) :: ic
   type(ionisation_equilibrium_t) :: ie
   type(uv_background_t) :: uvb
   type(logger_t) :: logger

   integer :: i

   tester = .describe."drawing_init"

   units = units_factory_generate('SI')
   chemistry = chemistry_factory_generate('H+He')
   ic = initial_condition_factory_generate('uniform')
   ie = ionisation_equilibrium_factory_generate('CaseA-CIE')
   uvb = uv_background_factory_generate('HM12')
   logger = logger_factory_generate('unicode-plotting')
   draw = drawing_factory_generate('sphere-SI')

   thermo = thermo_base_factory_generate('diatomic')
   call rhyme_thermo_base_init(thermo, units, logger)

   do i = 1, NDIM
      draw%shapes%sphere%origin(i) = ic%box_lengths(i)%v/2
   end do
   draw%shapes%sphere%r = ic%box_lengths(1)%v/3

   call rhyme_color_init
   call rhyme_nombre_init
   call rhyme_units_init(units, logger)
   call rhyme_thermo_base_init(thermo, units, logger)
   call rhyme_uv_background_init(uvb, units, logger)
   call rhyme_initial_condition_init(ic, samr, units, logger)
   call rhyme_chemistry_init(chemistry, units, logger)
   call rhyme_ionisation_equilibrium_init(ie, units, chemistry, logger)
   call rhyme_ionisation_equilibrium_update_table(ie, chemistry, uvb, ic%redshift, logger)

   call rhyme_drawing_init(draw, units, samr, ic, logger, ie, chemistry)

   failed = tester%failed()
end function rhyme_drawing_init_test
