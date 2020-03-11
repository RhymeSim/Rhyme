logical function rhyme_hydro_base_primitive_to_conserved_test() result(failed)
   use rhyme_hydro_base_factory
   use rhyme_assertion

   implicit none

   type(assertion_t) :: hy_tester

   real(kind=8), dimension(cid%rho:cid%p) :: w, u, u_exp

   hy_tester = .describe."primitive_to_conserved"

   w = hy_factory%generate_primitive()
   u_exp = hy_factory%generate_conserved()

   call rhyme_hydro_base_primitive_to_conserved(w, hy_factory%e_int, u)

   call hy_tester%expect(.notToBeNaN.u)
   call hy_tester%expect(u.toBe.u_exp)

   failed = hy_tester%failed()
end function rhyme_hydro_base_primitive_to_conserved_test
