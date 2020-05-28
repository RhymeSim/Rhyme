submodule(rhyme_report) new_histogram_smod
contains
module subroutine rhyme_report_new_histogram(report, type)
   implicit none

   class(report_t), intent(inout) :: report
   integer, intent(in) :: type

   type(report_histogram_t), pointer :: pntr

   if (.not. associated(report%histograms)) then
      allocate (report%histograms)

      report%histograms%type = type
   else
      pntr => report%histograms

      do while (associated(pntr%next))
         pntr => pntr%next
      end do

      allocate (pntr%next)

      pntr%next%type = type
   end if
end subroutine rhyme_report_new_histogram
end submodule new_histogram_smod
