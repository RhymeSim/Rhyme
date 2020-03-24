submodule(rhyme_periodic_table) rhyme_mh_init_smod
contains
module subroutine rhyme_periodic_table_init(pt, logger)
   implicit none

   type(periodic_table_t), intent(inout) :: pt
   type(logger_t), intent(inout) :: logger

   character(len=128) :: pt_str

   call logger%begin_section('periodic_table')

   write (pt_str, *) pt
   call logger%log('', 'periodic_table', '=', [pt_str])

   call logger%end_section
end subroutine rhyme_periodic_table_init
end submodule rhyme_mh_init_smod
