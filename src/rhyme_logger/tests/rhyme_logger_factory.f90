module rhyme_logger_factory
   use rhyme_logger

contains

   function logger_factory_generate(factory_type) result(logger)
      implicit none

      character(len=*), intent(in) :: factory_type
      type(logger_t) :: logger

      logger%projection_axis = lgid%z
      logger%colormap = csid%rainbow

      if (factory_type == 'default') then
         call logger%log(':)')
      else if (factory_type == 'unicode-plotting') then
         logger%unicode_plotting = .true.
      else
         print *, 'Unknonw logger factory type!', factory_type
      end if
   end function logger_factory_generate
end module rhyme_logger_factory
