logical function rhyme_thermo_base_flux_test() result(failed)
   use rhyme_thermo_base_factory
   use rhyme_units_factory
   use rhyme_hydro_base_factory
   use rhyme_logger_factory
   use rhyme_assertion

   implicit none

   type(assertion_t) :: th_tester

   type(thermo_base_t) :: thermo
   type(units_t) :: units
   type(logger_t) :: logger
   real(kind=8) :: u(cid%rho:cid%e_tot), f(cid%rho:cid%p), f_exp(cid%rho:cid%p)
   integer :: axis = 0

   th_tester = .describe."flux"

   call rhyme_nombre_init

   u = hy_factory%generate_conserved()

   units = units_factory_generate('SI')
   logger = logger_factory_generate('default')

   thermo = thermo_base_factory_generate('diatomic')
   call rhyme_thermo_base_init(thermo, units, logger)

   f = rhyme_thermo_base_flux(u, axis)
   call th_tester%expect(.notToBeNaN.f)

   call rhyme_ideal_gas_flux(ig_gamma(thid%diatomic), u, axis, f_exp)
   call th_tester%expect(f.toBe.f_exp.within.15)

   f = calc_flux(u, axis)
   call th_tester%expect(f.toBe.f_exp.within.15)

   failed = th_tester%failed()
end function rhyme_thermo_base_flux_test
