module rhyme_deep_rs_factory
   use rhyme_deep_rs

contains

   function deep_rs_factory_generate(factory_type) result(drs)
      implicit none

      character(len=*), intent(in) :: factory_type

      type(deep_rs_t) :: drs

      select case (factory_type)
      case ('defulat')
         drs = deep_rs_t()
      case default
         print *, 'Unknown deep_rs factory type!', factory_type
      end select
   end function deep_rs_factory_generate
end module rhyme_deep_rs_factory
