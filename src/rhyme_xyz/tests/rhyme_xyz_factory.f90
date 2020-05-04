module rhyme_xyz_factory
   use rhyme_xyz

contains

   function xyz_factory_generate(factory_type) result(xxx)
      implicit none

      character(len=*), intent(in) :: factory_type

      type(xyz_t) :: xxx

      if (factory_type == 'default') then
         xxx = xyz_t()
      else
         print *, 'Unknown xyz factory type!', factory_type
      end if
   end function xyz_factory_generate
end module rhyme_xyz_factory
