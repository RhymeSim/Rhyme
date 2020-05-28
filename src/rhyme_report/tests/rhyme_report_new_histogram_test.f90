logical function rhyme_report_new_histogram_test() result(failed)
   use rhyme_report_factory
   use rhyme_assertion

   implicit none

   type(assertion_t) :: tester

   type(report_t) :: report

   tester = .describe.'report_new_histogram'

   call report%new_histogram(repid%abs_v)
   call report%new_histogram(repid%v2)

   call tester%expect(report%histograms%type.toBe.repid%abs_v.hint.'|v|')
   call tester%expect(report%histograms%next%type.toBe.repid%v2.hint.'v2')
   call tester%expect(associated(report%histograms%next%next) .toBe..false..hint.'not accosiated')

   failed = tester%failed()
end function rhyme_report_new_histogram_test
