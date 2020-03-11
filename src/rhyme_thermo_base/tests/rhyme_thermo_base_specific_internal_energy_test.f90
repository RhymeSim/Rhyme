logical function rhyme_thermo_base_specific_internal_energy_test() result(failed)
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

   integer :: gas_type

   th_tester = .describe."specific_internal_energy"

   call rhyme_nombre_init

   physics = ph_factory%generate()
   logger = log_factory%generate()
   u = hy_factory%generate_conserved()

   do gas_type = thid%monatomic, thid%polyatomic
      thermo = th_factory%generate(physics, gas_type)
      call rhyme_thermo_base_init(thermo, physics, logger)

      call th_tester%expect(.notToBeNaN.rhyme_thermo_base_specific_internal_energy(u))
      call th_tester%expect(rhyme_thermo_base_specific_internal_energy(u) &
                            .toBe.rhyme_ideal_gas_specific_internal_energy( &
                            ig_gamma(gas_type), th_factory%kb_amu, u) &
                            .within.15)
   end do

   call th_tester%expect(calc_sp_int_e(u) &
                         .toBe.rhyme_thermo_base_specific_internal_energy(u) &
                         .within.15)

   failed = th_tester%failed()
end function rhyme_thermo_base_specific_internal_energy_test
