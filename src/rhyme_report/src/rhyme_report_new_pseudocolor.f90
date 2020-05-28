submodule(rhyme_report) new_pseudocolor_smod
contains
module subroutine rhyme_report_new_pseudocolor(report, type)
   implicit none

   class(report_t), intent(inout) :: report
   integer, intent(in) :: type

   type(report_pseudocolor_t), pointer :: pntr

   if (.not. associated(report%pseudocolors)) then
      allocate (report%pseudocolors)

      report%pseudocolors%type = type
   else
      pntr => report%pseudocolors

      do while (associated(pntr%next))
         pntr => pntr%next
      end do

      allocate (pntr%next)

      pntr%next%type = type
   end if
end subroutine rhyme_report_new_pseudocolor
end submodule new_pseudocolor_smod
