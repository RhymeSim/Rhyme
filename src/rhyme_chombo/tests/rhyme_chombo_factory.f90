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

   function chombo_factory_generate_output(output_type) result(ch_output)
      implicit none

      character(len=*), intent(in) :: output_type
      type(chombo_output_t) :: ch_output

      ch_output%every = chid%unset
      ch_output%restart_backup_every = chid%unset

      if (output_type == 'log') then
         allocate (ch_output%rules)
         ch_output%rules%type = chid%log
         ch_output%rules%range = [0d0, 1d0]
         ch_output%rules%noutputs = 11

         allocate (ch_output%rules%next)
         ch_output%rules%next%type = chid%log
      else
         print *, 'Unknown chombo output type!', output_type
      end if
   end function chombo_factory_generate_output
end module rhyme_chombo_factory
