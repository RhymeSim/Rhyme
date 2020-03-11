logical function rhyme_ideal_gas_primitive_vars_to_conserved_test() result(failed)
   use rhyme_ideal_gas_factory
   use rhyme_hydro_base_factory
   use rhyme_assertion

   implicit none

   type(assertion_t) :: ig_tester

   real(kind=8) :: rho, v(NDIM), p
   real(kind=8) :: u_exp(cid%rho:cid%e_tot), u(cid%rho:cid%e_tot)

   ig_tester = .describe."primitive_vars_to_conserved"

   rho = hy_factory%rho
   v = hy_factory%v
   p = hy_factory%p

   u_exp = hy_factory%generate_conserved()

   call rhyme_ideal_gas_primitive_vars_to_conserved(hy_factory%g, rho, v, p, u)

   call ig_tester%expect(u_exp.toBe.u.within.15)

   failed = ig_tester%failed()
end function rhyme_ideal_gas_primitive_vars_to_conserved_test
