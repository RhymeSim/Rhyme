module rhyme_xyz_factory
   use rhyme_xyz

contains

   function xyz_factory_generate(factory_type) result(xxx)
      implicit none

      character(len=*), intent(in) :: factory_type

      type(xyz_t) :: xxx

      select case (factory_type)
      case ('defulat')
         xxx = xyz_t()
      case default
         print *, 'Unknown xyz factory type!', factory_type
      end select
   end function xyz_factory_generate
end module rhyme_xyz_factory
