submodule(rhyme_ionisation_equilibrium) rhyme_mh_init_smod
contains
module subroutine rhyme_ionisation_equilibrium_init(ie, logger)
   implicit none

   type(ionisation_equilibrium_t), intent(inout) :: ie
   type(logger_t), intent(inout) :: logger

   call logger%begin_section('ionisation_equilibrium')

   call logger%end_section
end subroutine rhyme_ionisation_equilibrium_init
end submodule rhyme_mh_init_smod
