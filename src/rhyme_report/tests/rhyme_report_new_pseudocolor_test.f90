logical function rhyme_report_new_pseudocolor_test() result(failed)
   use rhyme_report_factory
   use rhyme_assertion

   implicit none

   type(assertion_t) :: tester

   type(report_t) :: report

   tester = .describe.'report_new_pseudocolor'

   call report%new_psudocolor(repid%rho)
   call report%new_psudocolor(repid%rho_u)
   call report%new_psudocolor(repid%e_tot)
   call report%new_psudocolor(repid%temp)
   call report%new_psudocolor(repid%ntr_frac_0)

   call tester%expect(report%pseudocolors%type.toBe.repid%rho.hint.'rho')
   call tester%expect(report%pseudocolors%next%type.toBe.repid%rho_u.hint.'rho_u')
   call tester%expect(report%pseudocolors%next%next%type.toBe.repid%e_tot.hint.'e_tot')
   call tester%expect(report%pseudocolors%next%next%next%type.toBe.repid%temp.hint.'temp')
   call tester%expect(report%pseudocolors%next%next%next%next%type.toBe.repid%ntr_frac_0.hint.'ntr_frac')
   call tester%expect(associated(report%pseudocolors%next%next%next%next%next) .toBe..false..hint.'not accosiated')

   failed = tester%failed()
end function rhyme_report_new_pseudocolor_test
