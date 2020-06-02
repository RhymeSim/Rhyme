logical function rhyme_ionisation_equilibrium_update_table_test() result(failed)
   use rhyme_ionisation_equilibrium_factory
   use rhyme_units_factory
   use rhyme_chemistry_factory
   use rhyme_uv_background_factory
   use rhyme_logger_factory
   use rhyme_assertion

   implicit none

   type(assertion_t) :: tester

   type(ionisation_equilibrium_t) :: ie
   type(units_t) :: units
   type(chemistry_t) :: chemistry
   type(uv_background_t) :: uvb
   type(logger_t) :: logger
   real(kind=8) :: z

   tester = .describe.'ionisation_equilibrium_update_table'

   ie = ionisation_equilibrium_factory_generate('CaseB-CIE')
   units = units_factory_generate('radamesh')
   chemistry = chemistry_factory_generate('H+He')
   uvb = uv_background_factory_generate('HM12')
   logger = logger_factory_generate('default')

   call rhyme_nombre_init
   call rhyme_color_init
   call rhyme_units_init(units, logger)
   call rhyme_chemistry_init(chemistry, units, logger)
   call rhyme_uv_background_init(uvb, units, logger)

   call rhyme_ionisation_equilibrium_init(ie, units, chemistry, logger)

   z = 1.37d0
   call rhyme_ionisation_equilibrium_update_table(ie, chemistry, uvb, z, logger)
   call tester%expect(ie%table_redhsift.toBe.z.hint.'table redshift')

   failed = tester%failed()
end function rhyme_ionisation_equilibrium_update_table_test
