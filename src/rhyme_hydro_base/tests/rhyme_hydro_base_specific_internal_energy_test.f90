logical function rhyme_hydro_base_specific_internal_energy_test() result(failed)
   use rhyme_hydro_base_factory
   use rhyme_assertion

   implicit none

   type(assertion_t) :: hy_tester

   real(kind=8) :: sp_int_e, v2, u(cid%rho:cid%e_tot)

   hy_tester = .describe."specific_internal_energy"

   u = hy_factory%generate_conserved()

   v2 = hy_factory%v(1)**2
#if NDIM > 1
   v2 = v2 + hy_factory%v(2)**2
#endif
#if NDIM > 2
   v2 = v2 + hy_factory%v(3)**2
#endif

   sp_int_e = rhyme_hydro_base_specific_internal_energy(u)

   call hy_tester%expect(.notToBeNaN.sp_int_e)
   call hy_tester%expect(sp_int_e.toBe.hy_factory%e_int/hy_factory%rho.within.15)

   failed = hy_tester%failed()
end function rhyme_hydro_base_specific_internal_energy_test
