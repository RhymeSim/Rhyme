submodule(rhyme_report) publish_smod
contains
module subroutine rhyme_report_publish(report, samr, logger)
   implicit none

   class(report_t), intent(in) :: report
   type(samr_t), intent(in) :: samr
   type(logger_t), intent(inout) :: logger

   call rhyme_report_plot_pseudocolors(report, samr, logger)
   call rhyme_report_plot_phase_diagrams(report, samr, logger)
   call rhyme_report_plot_histograms(report, samr, logger)
end subroutine rhyme_report_publish
end submodule publish_smod
