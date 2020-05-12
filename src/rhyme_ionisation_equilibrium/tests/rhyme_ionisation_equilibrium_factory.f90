module rhyme_ionisation_equilibrium_factory
   use rhyme_ionisation_equilibrium

contains

   function ionisation_equilibrium_factory_generate(factory_type) result(ie)
      implicit none

      character(len=*), intent(in) :: factory_type

      type(ionisation_equilibrium_t) :: ie

      if (factory_type == 'empty') then
      else if (factory_type == 'CaseA') then
         ie%cases = ieid%case_a
         ie%uvb = .false.
         ie%collisional = .false.
         ie%photo = .false.
      else if (factory_type == 'CaseA-cgs') then
         ie%cases = ieid%case_a
         ie%uvb = .false.
         ie%collisional = .true.
         ie%photo = .false.
         ie%table_sizes = [32, 32]
         ie%table_temp_range(:)%v = [1e2, 1e7]
         ie%table_temp_unit_str = 'K'
         ie%table_density_range(:)%v = [1e-2, 1e3]
         ie%table_density_unit_str = 'm_H / cm^3'
      else if (factory_type == 'CaseB') then
         ie%cases = ieid%case_b
         ie%uvb = .false.
         ie%collisional = .false.
         ie%photo = .false.
      else
         print *, 'Unknonw ionization equilibrium factory type!', factory_type
      end if
   end function ionisation_equilibrium_factory_generate
end module rhyme_ionisation_equilibrium_factory
