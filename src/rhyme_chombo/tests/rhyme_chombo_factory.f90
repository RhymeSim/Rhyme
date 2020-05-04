module rhyme_chombo_factory
   use rhyme_chombo

contains

   function chombo_factory_generate(factory_type) result(ch)
      implicit none

      character(len=*), intent(in) :: factory_type
      type(chombo_t) :: ch

      if (factory_type == 'empty') then
         ch%is_opened = .false.
         ch%num_levels = chid%unset
         ch%num_components = chid%unset
         ch%iteration = chid%unset
         ch%chombo_global_id = chid%unset
         ch%level_ids = chid%unset
         ch%prefix = ''
         ch%nickname = ''
      else
         print *, 'Unknown chombo factory type!', factory_type
      end if

   end function chombo_factory_generate
end module rhyme_chombo_factory
