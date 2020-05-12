submodule(rhyme_chemistry) rhyme_mh_init_smod
contains
module subroutine rhyme_chemistry_init(chem, logger)
   implicit none

   type(chemistry_t), intent(inout) :: chem
   type(logger_t), intent(inout) :: logger

   integer :: si

   call logger%begin_section('chemistry')

   call rhyme_periodic_table_init(chem%pt, logger)

   do si = 1, size(chem%species_name)
      chem%species(si) = chem%pt.getspeciesbyname.chem%species_name(si)
   end do

   call logger%end_section
end subroutine rhyme_chemistry_init
end submodule rhyme_mh_init_smod
