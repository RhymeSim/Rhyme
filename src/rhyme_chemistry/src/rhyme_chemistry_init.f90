submodule(rhyme_chemistry) rhyme_mh_init_smod
contains
module subroutine rhyme_chemistry_init(chem, logger)
   implicit none

   type(chemistry_t), intent(inout) :: chem
   type(logger_t), intent(inout) :: logger

   character(len=128) :: chem_str

   call logger%begin_section('chemistry')

   write (chem_str, *) chem
   call logger%log('', 'chemistry', '=', [chem_str])

   call logger%end_section
end subroutine rhyme_chemistry_init
end submodule rhyme_mh_init_smod
