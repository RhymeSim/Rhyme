logical function rhyme_ideal_gas_conserved_to_primitive_test() result(failed)
   use rhyme_ideal_gas_factory
   use rhyme_hydro_base_factory
   use rhyme_assertion

   implicit none

   type(assertion_t) :: tester

   real(kind=8) :: u(cid%rho:cid%e_tot), w(cid%rho:cid%p), p

   tester = .describe."conserved_to_primitive_smod"

   u = hy_factory%generate_conserved()

   call rhyme_ideal_gas_conserved_to_primitive(2d0, u, w)

   call tester%expect(w(cid%rho) .toBe.u(cid%rho) .hint.'rho')

   call tester%expect(w(cid%u) .toBe.u(cid%rho_u)/u(cid%rho) .within.15.hint.'u')
#if NDIM > 1
   call tester%expect(w(cid%v) .toBe.u(cid%rho_v)/u(cid%rho) .within.15.hint.'v')
#endif
#if NDIM > 2
   call tester%expect(w(cid%w) .toBe.u(cid%rho_w)/u(cid%rho) .within.15.hint.'w')
#endif

   p = (u(cid%e_tot) - .5d0*(sum(u(cid%rho_u:cid%rho_u + NDIM - 1)**2))/u(cid%rho)**2)*1d0
   call tester%expect(w(cid%p) .toBe.p.within.15.hint.'p')

   failed = tester%failed()
end function rhyme_ideal_gas_conserved_to_primitive_test
