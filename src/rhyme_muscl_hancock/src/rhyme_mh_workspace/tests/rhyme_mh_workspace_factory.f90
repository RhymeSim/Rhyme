module rhyme_mh_workspace_factory
   use rhyme_mh_workspace

contains

   function mh_workspace_factory_generate(factory_type) result(mhws)
      implicit none

      character(len=*), intent(in) :: factory_type
      type(mh_workspace_t) :: mhws

      mhws%nlevels = 1

      if (factory_type == 'memory_intensive') then
         mhws%type = mhwsid%memory_intensive
      else if (factory_type == 'cpu_intensive') then
         mhws%type = mhwsid%cpu_intensive
      else
         print *, 'Unknonw MH workspace factory type!', factory_type
      end if
   end function mh_workspace_factory_generate
end module rhyme_mh_workspace_factory
