logical function rhyme_ideal_gas_pressure_test() result(failed)
   use rhyme_ideal_gas_factory
   use rhyme_hydro_base_factory
   use rhyme_assertion

   implicit none

   type(assertion_t) :: ig_tester

   real(kind=8) :: p, u(cid%rho:cid%e_tot)

   ig_tester = .describe."pressure"

   u = hy_factory%generate_conserved()

   p = rhyme_ideal_gas_pressure(hy_factory%g, u)

   call ig_tester%expect(.notToBeNaN.p)
   call ig_tester%expect(p.toBe.hy_factory%p.within.15)

   failed = ig_tester%failed()
end function rhyme_ideal_gas_pressure_test
