module rhyme_physics_factory
   use rhyme_physics

   implicit none

   character(len=32), parameter, private :: rho_str_param = 'kg / m^3'
   character(len=32), parameter, private :: length_str_param = 'm'
   character(len=32), parameter, private :: time_str_param = 's'

contains
   function physics_factory_generate(factory_type) result(physics)
      implicit none

      character(len=*), intent(in) :: factory_type

      type(physics_t) :: physics

      if (factory_type == 'SI') then
         physics%rho_str = rho_str_param
         physics%length_str = length_str_param
         physics%time_str = time_str_param
      else
         print *, 'Unknow physcis factory type!'
      end if
   end function physics_factory_generate
end module rhyme_physics_factory
