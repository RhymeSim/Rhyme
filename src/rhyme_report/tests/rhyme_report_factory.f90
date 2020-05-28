module rhyme_report_factory
   use rhyme_report

contains

   function report_factory_generate(factory_type) result(rep)
      implicit none

      character(len=*), intent(in) :: factory_type

      type(report_t) :: rep

      if (factory_type == 'pseudocolors') then
         call rep%new_psudocolor(repid%rho)
      else if (factory_type == 'phase_diagrams') then
         call rep%new_phase_diagram(repid%rho_temp)
      else if (factory_type == 'histograms') then
         call rep%new_histogram(repid%v2)
      else
         print *, 'Unknown report factory type!', factory_type
      end if
   end function report_factory_generate
end module rhyme_report_factory
