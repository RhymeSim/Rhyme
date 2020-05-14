logical function rhyme_uv_background_h_self_shielding_n_test() result(failed)
   use rhyme_uv_background_factory
   use rhyme_physics_factory
   use rhyme_logger_factory
   use rhyme_assertion

   implicit none

   type(assertion_t) :: tester

   type(uv_background_t) :: uvb
   type(physics_t) :: physics
   type(logger_t) :: logger

   real(kind=8) :: z, n, n_expected

   tester = .describe."uv_background_h_self_shielding_n"

   uvb = uv_background_factory_generate('HM12')
   physics = physics_factory_generate('SI')
   logger = logger_factory_generate('default')

   z = 1.37d0

   call rhyme_nombre_init()
   call rhyme_physics_init(physics, logger)
   call rhyme_uv_background_init(uvb, physics, logger)

   n = rhyme_uv_background_h_self_shielding_n(uvb, z, logger)
   n_expected = 5.101654214551882d-03*uvb%rho_to_code_unit

   call tester%expect(n.toBe.n_expected.hint.'SSh density')

   failed = tester%failed()
end function rhyme_uv_background_h_self_shielding_n_test
