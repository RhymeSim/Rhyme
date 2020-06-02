logical function rhyme_units_test() result(failed)
   use rhyme_units
   use rhyme_assertion

   implicit none

   type(assertion_t) :: ph_tester

   type(units_t) :: units

   ph_tester = .describe."rhyme_units"

   call ph_tester%expect(cid%rho.toBe.1.hint.'hydro, rho')
   call ph_tester%expect(cid%u.toBe.2.hint.'hydro, u')
   call ph_tester%expect(cid%rho_u.toBe.2.hint.'hydro, rho_u')
#if NDIM > 1
   call ph_tester%expect(cid%v.toBe.3.hint.'hydro, v')
   call ph_tester%expect(cid%rho_v.toBe.3.hint.'hydro, rho_v')
#endif
#if NDIM > 2
   call ph_tester%expect(cid%w.toBe.4.hint.'hydro, w')
   call ph_tester%expect(cid%rho_w.toBe.4.hint.'hydro, rho_w')
#endif
   call ph_tester%expect(cid%e_tot.toBe.1 + NDIM + 1.hint.'hydro, e_tot')
   call ph_tester%expect(cid%p.toBe.1 + NDIM + 1.hint.'hydro, p')
   call ph_tester%expect(cid%temp.toBe.1 + NDIM + 1 + 1.hint.'rhd, temp')
   call ph_tester%expect(cid%ntr_frac_0.toBe.1 + NDIM + 1 + 2.hint.'rhd, ntr_frac_0')
#if NSPE > 1
   call ph_tester%expect(cid%ntr_frac_1.toBe.1 + NDIM + 1 + 3.hint.'rhd, ntr_frac_1')
#endif
#if NSPE > 2
   call ph_tester%expect(cid%ntr_frac_2.toBe.1 + NDIM + 1 + 4.hint.'rhd, ntr_frac_2')
#endif

   call ph_tester%expect(associated(units%rho) .toBe..false.)
   call ph_tester%expect(associated(units%length) .toBe..false.)
   call ph_tester%expect(associated(units%time) .toBe..false.)
   call ph_tester%expect(associated(units%pressure) .toBe..false.)
   call ph_tester%expect(associated(units%temperature) .toBe..false.)

   failed = ph_tester%failed()
end function rhyme_units_test
