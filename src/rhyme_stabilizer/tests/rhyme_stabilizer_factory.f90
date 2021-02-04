module rhyme_stabilizer_factory
   use rhyme_stabilizer

contains

   function stabilizer_factory_generate(factory_type) result(st)
      implicit none

      character(len=*), intent(in) :: factory_type

      type(stabilizer_t) :: st

      select case (factory_type)
      case ('defulat')
         st = stabilizer_t()
      case default
         print *, 'Unknown stabilizer factory type!', factory_type
      end select
   end function stabilizer_factory_generate
end module rhyme_stabilizer_factory
