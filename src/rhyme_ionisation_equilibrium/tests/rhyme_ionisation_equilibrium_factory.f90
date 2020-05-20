module rhyme_ionisation_equilibrium_factory
   use rhyme_ionisation_equilibrium

contains

   function ionisation_equilibrium_factory_generate(factory_type) result(ie)
      implicit none

      character(len=*), intent(in) :: factory_type

      type(ionisation_equilibrium_t) :: ie

      if (factory_type == 'empty') then
      else if (factory_type == 'CaseA-CIE') then
         ie%cases = ieid%case_a
         ie%uvb = .false.
         ie%uvb_self_shielding = .false.
         ie%collisional = .true.
         ie%photo = .false.
         ie%table_sizes = [1024, 1024]
         ie%table_temp_range(1)%v = 1d2
         ie%table_temp_range(2)%v = 1d8
         ie%table_temp_unit_str = 'K'
         ie%table_density_range(1)%v = 1d-4
         ie%table_density_range(2)%v = 1d3
         ie%table_density_unit_str = 'm_H / cm^3'
         ie%convergence_rate = .01
         ie%max_niterations = 1000
      else if (factory_type == 'CaseA-CPIE') then
         ie%cases = ieid%case_a
         ie%uvb = .true.
         ie%uvb_self_shielding = .true.
         ie%collisional = .true.
         ie%photo = .false.
         ie%table_sizes = [1024, 1024]
         ie%table_temp_range(1)%v = 1d2
         ie%table_temp_range(2)%v = 1d8
         ie%table_temp_unit_str = 'K'
         ie%table_density_range(1)%v = 1d-4
         ie%table_density_range(2)%v = 1d3
         ie%table_density_unit_str = 'm_H / cm^3'
         ie%convergence_rate = .01
         ie%max_niterations = 1000
      else if (factory_type == 'CaseB-CIE') then
         ie%cases = ieid%case_b
         ie%uvb = .false.
         ie%uvb_self_shielding = .false.
         ie%collisional = .true.
         ie%photo = .false.
         ie%table_sizes = [1024, 1024]
         ie%table_temp_range(1)%v = 1d3
         ie%table_temp_range(2)%v = 1d8
         ie%table_temp_unit_str = 'K'
         ie%table_density_range(1)%v = 1d-4
         ie%table_density_range(2)%v = 1d3
         ie%table_density_unit_str = 'm_H / cm^3'
         ie%convergence_rate = .01
         ie%max_niterations = 1000
      else if (factory_type == 'CaseA-CPIE') then
         ie%cases = ieid%case_a
         ie%uvb = .true.
         ie%uvb_self_shielding = .true.
         ie%collisional = .true.
         ie%photo = .false.
         ie%table_sizes = [128, 128]
         ie%table_temp_range(:)%v = [1e2, 1e7]
         ie%table_temp_unit_str = 'K'
         ie%table_density_range(:)%v = [1e-2, 1e3]
         ie%table_density_unit_str = 'm_H / cm^3'
         ie%convergence_rate = .01
         ie%max_niterations = 1000
      else
         print *, 'Unknonw ionization equilibrium factory type!', factory_type
      end if
   end function ionisation_equilibrium_factory_generate
end module rhyme_ionisation_equilibrium_factory
