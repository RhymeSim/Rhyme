logical function rhyme_thermo_base_temperature_per_mu_test() result(failed)
   use rhyme_thermo_base_factory
   use rhyme_units_factory
   use rhyme_hydro_base_factory
   use rhyme_logger_factory
   use rhyme_assertion

   implicit none

   type(assertion_t) :: th_tester

   type(units_t) :: units
   type(thermo_base_t) :: thermo
   type(logger_t) :: logger
   real(kind=8) :: u(cid%rho:cid%e_tot)

   th_tester = .describe."temperature_per_mu"

   call rhyme_nombre_init

   units = units_factory_generate('SI')
   logger = logger_factory_generate('default')

   u = hy_factory%generate_conserved()

   thermo = thermo_base_factory_generate('diatomic')
   call rhyme_thermo_base_init(thermo, units, logger)
   call th_tester%expect(.notToBeNaN.rhyme_thermo_base_temperature_per_mu(u))
   call th_tester%expect( &
      rhyme_thermo_base_temperature_per_mu(u) &
      .toBe.rhyme_ideal_gas_temperature_per_mu( &
      ig_gamma(thid%diatomic), units%kb%v/units%amu%v, u) .within.15)

   call th_tester%expect( &
      calc_t_mu(u) &
      .toBe.rhyme_thermo_base_temperature_per_mu(u) .within.15)

   failed = th_tester%failed()
end function rhyme_thermo_base_temperature_per_mu_test
