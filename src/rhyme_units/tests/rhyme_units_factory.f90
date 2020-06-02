module rhyme_units_factory
   use rhyme_units
contains
   function units_factory_generate(factory_type) result(units)
      implicit none

      character(len=*), intent(in) :: factory_type

      type(units_t) :: units

      if (factory_type == 'SI') then
         units%rho_str = 'kg / m^3'
         units%length_str = 'm'
         units%time_str = 's'
      else if (factory_type == 'radamesh') then
         units%rho_str = 'm_H / cm^3'
         units%length_str = 'Mpc'
         units%time_str = 'Myr'
      else
         print *, 'Unknow physcis factory type!'
      end if
   end function units_factory_generate
end module rhyme_units_factory
