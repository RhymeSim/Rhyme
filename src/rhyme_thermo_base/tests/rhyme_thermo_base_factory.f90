module rhyme_thermo_base_factory
   use rhyme_thermo_base

contains

   function thermo_base_factory_generate(factory_type) result(thermo)
      implicit none

      character(len=*), intent(in) :: factory_type

      type(thermo_base_t) :: thermo

      if (factory_type == 'monatomic') then
         thermo%state_of_matter = thid%monatomic
      else if (factory_type == 'diatomic') then
         thermo%state_of_matter = thid%diatomic
      else if (factory_type == 'polyatomic') then
         thermo%state_of_matter = thid%polyatomic
      else
         print *, 'Unknown thermodynamic factory type!', factory_type
      end if
   end function thermo_base_factory_generate
end module rhyme_thermo_base_factory
