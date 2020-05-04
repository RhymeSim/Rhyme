module rhyme_muscl_hancock_factory
   use rhyme_muscl_hancock

contains

   function muscl_hancock_factory_generate(factory_type) result(mh)
      implicit none

      character(len=*), intent(in) :: factory_type

      type(muscl_hancock_t) :: mh

      if (factory_type == 'memory_intensive') then
         mh%solver_type = mhid%memory_intensive
      else if (factory_type == 'cpu_intensive') then
         mh%solver_type = mhid%cpu_intensive
      else
         print *, 'Unknonw MUSCL-Hancock factory type!', factory_type
      end if
   end function muscl_hancock_factory_generate
end module rhyme_muscl_hancock_factory
