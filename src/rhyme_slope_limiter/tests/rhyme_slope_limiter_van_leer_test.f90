logical function rhyme_slope_limiter_van_leer_test() result(failed)
   use rhyme_slope_limiter_factory
   use rhyme_hydro_base_factory
   use rhyme_assertion

   implicit none

   type(assertion_t) :: sl_tester

   type(slope_limiter_t) :: sl
   real(kind=8), dimension(cid%rho:cid%e_tot) :: delta, cons, ul, ur, um

   sl_tester = .describe."slope_limiter_van_leer"

   cons = hy_factory%generate_conserved()

   ul = cons
   ur = cons

   um = cons*1.23d0
   call rhyme_slope_limiter_van_leer(sl, ul, um, ur, delta)
   call sl_tester%expect(delta.toBe.0.d0)

   um = cons*(-2.34)
   call rhyme_slope_limiter_van_leer(sl, ul, um, ur, delta)
   call sl_tester%expect(delta.toBe.0.d0)

   um = cons*1.23d0
   ur = cons*2.34d0
   call rhyme_slope_limiter_van_leer(sl, ul, um, ur, delta)
   call sl_tester%expect(delta.notToBe.0.d0)

   um = cons*(-1.23d0)
   ur = cons*(-2.34d0)
   call rhyme_slope_limiter_van_leer(sl, ul, um, ur, delta)
   call sl_tester%expect(delta.notToBe.0.d0)

   failed = sl_tester%failed()
end function rhyme_slope_limiter_van_leer_test
