module rhyme_ionisation_equilibrium_factory
   use rhyme_ionisation_equilibrium

contains

   function ionisation_equilibrium_factory_generate(factory_type) result(ie)
      implicit none

      character(len=*), intent(in) :: factory_type

      type(ionisation_equilibrium_t) :: ie

      if (factory_type == 'empty') then
         ie = ionisation_equilibrium_t(ieid%unset, .false., .false., .false.)
      else if (factory_type == 'default') then
         ie = ionisation_equilibrium_t()
      else if (factory_type == 'CaseA') then
         ie = ionisation_equilibrium_t(ieid%case_a, .false., .false., .false.)
      else if (factory_type == 'CaseB') then
         ie = ionisation_equilibrium_t(ieid%case_b, .false., .false., .false.)
      else
         print *, 'Unknonw ionization equilibrium factory type!', factory_type
      end if
   end function ionisation_equilibrium_factory_generate
end module rhyme_ionisation_equilibrium_factory
