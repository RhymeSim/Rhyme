logical function rhyme_thermo_base_pressure_test() result(failed)
   use rhyme_thermo_base_factory
   use rhyme_physics_factory
   use rhyme_hydro_base_factory
   use rhyme_logger_factory
   use rhyme_assertion

   implicit none

   type(assertion_t) :: th_tester

   type(physics_t) :: physics
   type(thermo_base_t) :: thermo
   type(logger_t) :: logger
   real(kind=8) :: u(cid%rho:cid%e_tot)

   th_tester = .describe."pressure"

   call rhyme_nombre_init

   physics = physics_factory_generate('SI')
   logger = logger_factory_generate('default')

   u = hy_factory%generate_conserved()

   thermo = thermo_base_factory_generate('diatomic')
   call rhyme_thermo_base_init(thermo, physics, logger)
   call th_tester%expect(.notToBeNaN.rhyme_thermo_base_pressure(u))
   call th_tester%expect( &
      rhyme_thermo_base_pressure(u) .toBe.rhyme_ideal_gas_pressure( &
      ig_gamma(thid%diatomic), physics%kb%v/physics%amu%v, u) .within.15)

   call th_tester%expect(calc_p(u) .toBe.rhyme_thermo_base_pressure(u) .within.15)

   failed = th_tester%failed()
end function rhyme_thermo_base_pressure_test
