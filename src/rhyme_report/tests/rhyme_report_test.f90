logical function rhyme_report_test() result(failed)
   use rhyme_report_factory
   use rhyme_assertion

   implicit none

   type(assertion_t) :: tester

   type(report_t) :: rep

   tester = .describe."report"

   rep = report_factory_generate('default')

   call tester%expect(repid%p_temp.toBe.-7.hint.'p_temp')
   call tester%expect(repid%labels(repid%p_temp) .toBe.'p vs temp'.hint.'p_temp')

   call tester%expect(repid%rho_temp.toBe.-6.hint.'rho_temp')
   call tester%expect(repid%labels(repid%rho_temp) .toBe.'rho vs temp'.hint.'rho_temp')

   call tester%expect(repid%v2.toBe.-5.hint.'v2')
   call tester%expect(repid%labels(repid%v2) .toBe.'v2'.hint.'v2')

   call tester%expect(repid%abs_v.toBe.-4.hint.'|v|')
   call tester%expect(repid%labels(repid%abs_v) .toBe.'|v|'.hint.'|v|')

   call tester%expect(repid%w.toBe.-3.hint.'w')
   call tester%expect(repid%labels(repid%w) .toBe.'w'.hint.'w')

   call tester%expect(repid%v.toBe.-2.hint.'v')
   call tester%expect(repid%labels(repid%v) .toBe.'v'.hint.'v')

   call tester%expect(repid%u.toBe.-1.hint.'u')
   call tester%expect(repid%labels(repid%u) .toBe.'u'.hint.'u')

   call tester%expect(repid%unset.toBe.0.hint.'unset')

   call tester%expect(repid%rho.toBe.1.hint.'rho')
   call tester%expect(repid%labels(repid%rho) .toBe.'rho'.hint.'rho')

   call tester%expect(repid%rho_u.toBe.2.hint.'rho_u')
   call tester%expect(repid%labels(repid%rho_u) .toBe.'rho_u'.hint.'rho_u')

#if NDIM > 1
   call tester%expect(repid%rho_v.toBe.3.hint.'rho_v')
   call tester%expect(repid%labels(repid%rho_v) .toBe.'rho_v'.hint.'rho_v')
#endif

#if NDIM > 1
   call tester%expect(repid%rho_w.toBe.4.hint.'rho_w')
   call tester%expect(repid%labels(repid%rho_w) .toBe.'rho_w'.hint.'rho_v')
#endif

   call tester%expect(repid%e_tot.toBe.1 + NDIM + 1.hint.'e_tot')
   call tester%expect(repid%labels(repid%e_tot) .toBe.'e_tot'.hint.'e_tot')

   call tester%expect(repid%temp.toBe.1 + NDIM + 2.hint.'temp')
   call tester%expect(repid%labels(repid%temp) .toBe.'temp'.hint.'temp')

   call tester%expect(repid%ntr_frac_0.toBe.1 + NDIM + 3.hint.'ntr_frac_0')
   call tester%expect(repid%labels(repid%ntr_frac_0) .toBe.'ntr_frac_0'.hint.'ntr_frac_0')

#if NSPE > 1
   call tester%expect(repid%ntr_frac_1.toBe.1 + NDIM + 4.hint.'ntr_frac_1')
   call tester%expect(repid%labels(repid%ntr_frac_1) .toBe.'ntr_frac_1'.hint.'ntr_frac_1')
#endif

#if NSPE > 2
   call tester%expect(repid%ntr_frac_2.toBe.1 + NDIM + 5.hint.'ntr_frac_2')
   call tester%expect(repid%labels(repid%ntr_frac_2) .toBe.'ntr_frac_2'.hint.'ntr_frac_2')
#endif

   failed = tester%failed()
end function rhyme_report_test
