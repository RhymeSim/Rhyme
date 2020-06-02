logical function rhyme_uv_background_init_test() result(failed)
   use rhyme_uv_background_factory
   use rhyme_units_factory
   use rhyme_logger_factory
   use rhyme_assertion

   implicit none

   type(assertion_t) :: tester

   type(uv_background_t) :: uvb
   type(units_t) :: units
   type(logger_t) :: logger

   tester = .describe."uv_background_init"

   units = units_factory_generate('SI')
   logger = logger_factory_generate('default')

   uvb = uv_background_factory_generate('HM12')

   call rhyme_nombre_init()
   call rhyme_units_init(units, logger)

   call rhyme_uv_background_init(uvb, units, logger)

   call tester%expect(uvb%rho_to_code_unit.toBe. (hydrogen_mass%conv*1d6) .hint.'UVB rho to unit code factor')

   failed = tester%failed()
end function rhyme_uv_background_init_test
