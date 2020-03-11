logical function rhyme_ideal_gas_specific_internal_energy_test() result(failed)
   use rhyme_ideal_gas_factory
   use rhyme_hydro_base_factory
   use rhyme_assertion

   implicit none

   type(assertion_t) :: ig_tester

   real(kind=8) :: e_int, e_int_exp, u(cid%rho:cid%e_tot)

   ig_tester = .describe."specific_internal_energy"

   u = hy_factory%generate_conserved()

   e_int = rhyme_ideal_gas_specific_internal_energy(hy_factory%g, hy_factory%kb_amu, u)
   e_int_exp = hy_factory%e_int/hy_factory%rho

   call ig_tester%expect(.notToBeNaN.e_int)
   call ig_tester%expect(e_int.toBe.e_int_exp.within.15)

   failed = ig_tester%failed()
end function rhyme_ideal_gas_specific_internal_energy_test
