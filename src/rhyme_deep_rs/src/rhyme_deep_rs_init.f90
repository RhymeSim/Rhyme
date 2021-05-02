submodule(rhyme_deep_rs) init_smod
contains
   module subroutine rhyme_deep_rs_init(drs, logger)
      implicit none

      type(deep_rs_t), intent(inout) :: drs
      type(logger_t), intent(inout) :: logger

      character(len=128) :: drs_str

      call logger%begin_section('deep_rs')

      write (drs_str, *) drs
      call logger%log('', 'deep_rs', '=', [drs_str])

      call logger%end_section
   end subroutine rhyme_deep_rs_init
end submodule init_smod
