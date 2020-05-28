logical function rhyme_report_new_2d_histogram_test() result(failed)
   use rhyme_report_factory
   use rhyme_assertion

   implicit none

   type(assertion_t) :: tester

   type(report_t) :: report

   tester = .describe.'report_new_2d_histogram'

   call report%new_phase_diagram(repid%rho_temp)
   call report%new_phase_diagram(repid%p_temp)

   call tester%expect(report%phase_diagrams%type.toBe.repid%rho_temp.hint.'rho_temp')
   call tester%expect(report%phase_diagrams%next%type.toBe.repid%p_temp.hint.'p_temp')
   call tester%expect(associated(report%phase_diagrams%next%next) .toBe..false..hint.'not accosiated')

   failed = tester%failed()
end function rhyme_report_new_2d_histogram_test
