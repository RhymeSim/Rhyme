submodule(rhyme_xyz) init_smod
contains
   module subroutine rhyme_xyz_init(xxx, logger)
      implicit none

      type(xyz_t), intent(inout) :: xxx
      type(logger_t), intent(inout) :: logger

      character(len=128) :: xxx_str

      call logger%begin_section('xyz')

      write (xxx_str, *) xxx
      call logger%log('', 'xyz', '=', [xxx_str])

      call logger%end_section
   end subroutine rhyme_xyz_init
end submodule init_smod
