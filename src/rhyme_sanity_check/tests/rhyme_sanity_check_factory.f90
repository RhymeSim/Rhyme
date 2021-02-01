module rhyme_sanity_check_factory
   use rhyme_sanity_check

contains

   function sanity_check_factory_generate(factory_type) result(sc)
      implicit none

      character(len=*), intent(in) :: factory_type

      type(sanity_check_t) :: sc

      select case (factory_type)
      case ('default')
         sc = sanity_check_t()
         sc%properties = .true.
         sc%rho_unit_str = 'kg / m^3'
         sc%vx_unit_str = 'm / s'
         sc%vy_unit_str = 'm / s'
         sc%vz_unit_str = 'm / s'
         sc%e_tot_unit_str = 'kg * m^2 / s^2'
         sc%temp_unit_str = 'K'
      case default
         print *, 'Unknown sanity_check factory type!', factory_type
      end select
   end function sanity_check_factory_generate
end module rhyme_sanity_check_factory
