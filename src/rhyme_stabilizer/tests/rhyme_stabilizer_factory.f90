module rhyme_stabilizer_factory
   use rhyme_stabilizer

contains

   function stabilizer_factory_generate(factory_type) result(st)
      implicit none

      character(len=*), intent(in) :: factory_type

      type(stabilizer_t) :: st

      st = stabilizer_t()
      st%enabled = .true.

      select case (factory_type)
      case ('linear-rho^2')
         st%weight = cid%rho
         st%weight_power = 2
         st%extrapolation_type = stid%linear
         st%max_displacement = 2
         st%tolerance = 0d0
         st%initialize_target = .true.
         st%target_center = 1d1
      case ('quadratic')
         st%weight = cid%rho
         st%weight_power = 2
         st%extrapolation_type = stid%quadratic
         st%max_displacement = 2
         st%tolerance = 0d0
         st%initialize_target = .true.
         st%target_center = 1d1
      case ('default')
         st%extrapolation_type = stid%linear
         st%max_displacement = 1
      case default
         print *, 'Unknown stabilizer factory type!', factory_type
      end select
   end function stabilizer_factory_generate
end module rhyme_stabilizer_factory
