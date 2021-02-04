submodule(rhyme_stabilizer) init_smod
contains
   module subroutine rhyme_stabilizer_init(st, logger)
      implicit none

      type(stabilizer_t), intent(inout) :: st
      type(logger_t), intent(inout) :: logger

      character(len=128) :: st_str

      call logger%begin_section('stabilizer')

      write (st_str, *) st
      call logger%log('', 'stabilizer', '=', [st_str])

      call logger%end_section
   end subroutine rhyme_stabilizer_init
end submodule init_smod
