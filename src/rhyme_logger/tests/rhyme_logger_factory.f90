module rhyme_logger_factory
   use rhyme_logger

contains

   function logger_factory_generate(factory_type) result(logger)
      implicit none

      character(len=*), intent(in) :: factory_type
      type(logger_t) :: logger

      if (factory_type == 'default') then
         call logger%log(':)')
      else
         print *, 'Unknonw logger factory type!', factory_type
      end if
   end function logger_factory_generate
end module rhyme_logger_factory
