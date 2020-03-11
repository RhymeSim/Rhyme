logical function rhyme_thermo_base_primitive_vars_to_conserved_test() result(failed)
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
   real(kind=8), dimension(cid%rho:cid%e_tot) :: u, u_interface, u_exp
   real(kind=8) :: rho, v(NDIM), p
   integer :: gas_type

   th_tester = .describe."primitive_vars_to_conserved"

   call rhyme_nombre_init

   physics = ph_factory%generate()
   logger = log_factory%generate()
   rho = hy_factory%rho
   v = hy_factory%v
   p = hy_factory%p

   do gas_type = thid%monatomic, thid%polyatomic
      thermo = th_factory%generate(physics, gas_type)
      call rhyme_thermo_base_init(thermo, physics, logger)

      call rhyme_thermo_base_primitive_vars_to_conserved(rho, v, p, u)
      call rhyme_ideal_gas_primitive_vars_to_conserved(ig_gamma(gas_type), rho, v, p, u_exp)

      call th_tester%expect(.notToBeNaN.u)

      call th_tester%expect(u.toBe.u_exp.within.15)
   end do

   call conv_prim_vars_to_cons(rho, v, p, u_interface)
   call th_tester%expect(u_interface.toBe.u_exp.within.15)

   failed = th_tester%failed()
end function rhyme_thermo_base_primitive_vars_to_conserved_test
