module rhyme_physics_factory
   use rhyme_physics
contains
   function physics_factory_generate(factory_type) result(physics)
      implicit none

      character(len=*), intent(in) :: factory_type

      type(physics_t) :: physics

      if (factory_type == 'SI') then
         physics%rho_str = 'kg / m^3'
         physics%length_str = 'm'
         physics%time_str = 's'
      else if (factory_type == 'radamesh') then
         physics%rho_str = 'm_H / cm^3'
         physics%length_str = 'Mpc'
         physics%time_str = 'Myr'
      else
         print *, 'Unknow physcis factory type!'
      end if
   end function physics_factory_generate
end module rhyme_physics_factory
