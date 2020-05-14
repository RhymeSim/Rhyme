logical function rhyme_uv_background_init_test() result(failed)
   use rhyme_uv_background_factory
   use rhyme_physics_factory
   use rhyme_logger_factory
   use rhyme_assertion

   implicit none

   type(assertion_t) :: tester

   type(uv_background_t) :: uvb
   type(physics_t) :: physics
   type(logger_t) :: logger

   tester = .describe."uv_background_init"

   physics = physics_factory_generate('SI')
   logger = logger_factory_generate('default')

   uvb = uv_background_factory_generate('HM12')

   call rhyme_nombre_init()
   call rhyme_physics_init(physics, logger)

   call rhyme_uv_background_init(uvb, physics, logger)

   call tester%expect(uvb%rho_to_code_unit.toBe. (hydrogen_mass%conv*1d6) .hint.'UVB rho to unit code factor')

   failed = tester%failed()
end function rhyme_uv_background_init_test
