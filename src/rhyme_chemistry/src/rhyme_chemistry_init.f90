submodule(rhyme_chemistry) init_smod
contains
module subroutine rhyme_chemistry_init(chem, logger)
   implicit none

   type(chemistry_t), intent(inout) :: chem
   type(logger_t), intent(inout) :: logger

   integer :: si, ei
   integer :: nelements

   call logger%begin_section('chemistry')

   call rhyme_periodic_table_init(chem%pt, logger)

   nelements = 0

   do si = 1, NSPE
      chem%species(si) = chem%pt.getspeciesbyname.chem%species_names(si)
      ei = findloc(chem%element_names, chem%species(si)%element, dim=1)
      chem%species_abundances(si) = chem%element_abundances(ei)
   end do

   do ei = 1, NELE
      chem%elements(ei) = chem%pt.getspeciesbyname. (trim(chem%element_names(ei))//'I')
   end do

   call logger%end_section
end subroutine rhyme_chemistry_init
end submodule init_smod
