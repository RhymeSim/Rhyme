submodule(rhyme_report) rhyme_mh_init_smod
contains
module subroutine rhyme_report_init(rep, logger)
   implicit none

   type(report_t), intent(inout) :: rep
   type(logger_t), intent(inout) :: logger

   character(len=128) :: rep_str

   call logger%begin_section('report')

   write (rep_str, *) rep
   call logger%log('', 'report', '=', [rep_str])

   call logger%end_section
end subroutine rhyme_report_init
end submodule rhyme_mh_init_smod
