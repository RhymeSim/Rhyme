logical function rhyme_ideal_gas_primitive_to_conserved_test() result(failed)
   use rhyme_ideal_gas_factory
   use rhyme_hydro_base_factory
   use rhyme_assertion

   implicit none

   type(assertion_t) :: ig_tester

   real(kind=8) :: u(cid%rho:cid%e_tot), w(cid%rho:cid%p)

   ig_tester = .describe."primitive_to_conserved"

   w = hy_factory%generate_primitive()
   u = hy_factory%generate_conserved()

   call rhyme_ideal_gas_primitive_to_conserved(hy_factory%g, w, u)

   call ig_tester%expect(.notToBeNaN.u)

   failed = ig_tester%failed()
end function rhyme_ideal_gas_primitive_to_conserved_test
