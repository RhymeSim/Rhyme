logical function rhyme_muscl_hancock_half_step_extrapolation_test() result(failed)
   use rhyme_muscl_hancock_factory
   use rhyme_physics_factory
   use rhyme_thermo_base_factory
   use rhyme_logger_factory
   use rhyme_assertion

   implicit none

   type(assertion_t) :: tester

   type(physics_t) :: physics
   type(thermo_base_t) :: thermo
   type(logger_t) :: logger

   real(kind=8), dimension(cid%rho:cid%e_tot) :: u, delta, l, r, df

   tester = .describe."half_step_extrapolation"

   call rhyme_nombre_init

   physics = physics_factory_generate('SI')
   logger = logger_factory_generate('default')

   call rhyme_physics_init(physics, logger)

   thermo = thermo_base_factory_generate('diatomic')
   call rhyme_thermo_base_init(thermo, physics, logger)

   u = 1.d0
   delta = 0.d0

   call rhyme_muscl_hancock_half_step_extrapolation(u, delta, 1, 1.d0, 1.d0, l, r)

   call tester%expect(.notToBeNaN.l)
   call tester%expect(.notToBeNaN.r)
   call tester%expect(l.toBe.1.d0)
   call tester%expect(r.toBe.1.d0)

   u = 1.d0
   delta = 1.d0

   call rhyme_muscl_hancock_half_step_extrapolation(u, delta, 1, 1.d0, 1.d0, l, r)

   call tester%expect(.notToBeNaN.l)
   call tester%expect(.notToBeNaN.r)

   df = calc_flux(u - .5d0, 1) - calc_flux(u + .5d0, 1)
   call tester%expect(l.toBe. (1.d0 - .5d0) + .5d0*df)
   call tester%expect(r.toBe. (1.d0 + .5d0) + .5d0*df)

   failed = tester%failed()
end function rhyme_muscl_hancock_half_step_extrapolation_test
