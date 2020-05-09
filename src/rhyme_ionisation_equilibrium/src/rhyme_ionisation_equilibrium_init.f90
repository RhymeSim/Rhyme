submodule(rhyme_ionisation_equilibrium) rhyme_mh_init_smod
contains
module subroutine rhyme_ionisation_equilibrium_init(ie, chemistry, logger)
   implicit none

   type(ionisation_equilibrium_t), intent(inout) :: ie
   type(chemistry_t), intent(in) :: chemistry
   type(logger_t), intent(inout) :: logger

   integer :: si

   call logger%begin_section('ionisation_equilibrium')

   do si = 1, NSPE
      if (ie%cases(si) == ieid%case_a) then
         ie%RI(si)%run => chemistry%species(si)%s%RI_A
         ie%CIE(si)%run => chemistry%species(si)%s%CIE_A
      else if (ie%cases(si) == ieid%case_b) then
         ie%RI(si)%run => chemistry%species(si)%s%RI_B
         ie%CIE(si)%run => chemistry%species(si)%s%CIE_B
      else
         call logger%err('', 'Unknown case', ':', [ie%cases(si)])
      end if

      ie%CI(si)%run => chemistry%species(si)%s%CI
   end do

   call logger%end_section
end subroutine rhyme_ionisation_equilibrium_init
end submodule rhyme_mh_init_smod
