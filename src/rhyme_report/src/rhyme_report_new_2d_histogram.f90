submodule(rhyme_report) new_2d_histogram_smod
contains
module subroutine rhyme_report_new_2d_histogram(report, type)
   implicit none

   class(report_t), intent(inout) :: report
   integer, intent(in) :: type

   type(report_2d_histogram_t), pointer :: pntr

   if (.not. associated(report%phase_diagrams)) then
      allocate (report%phase_diagrams)

      report%phase_diagrams%type = type
   else
      pntr => report%phase_diagrams

      do while (associated(pntr%next))
         pntr => pntr%next
      end do

      allocate (pntr%next)

      pntr%next%type = type
   end if
end subroutine rhyme_report_new_2d_histogram
end submodule new_2d_histogram_smod
