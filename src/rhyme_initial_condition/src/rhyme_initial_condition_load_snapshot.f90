submodule(rhyme_initial_condition) rhyme_ic_load_snapshot_smod
contains
   module subroutine rhyme_initial_condition_load_snapshot(ic, samr, logger)
      implicit none

      type(initial_condition_t), intent(in) :: ic
      type(samr_t), intent(inout) :: samr
      type(logger_t), intent(inout) :: logger

      logical :: exist

      inquire (file=trim(ic%snapshot_path), exist=exist)
      if (.not. exist) then
         call logger%err('Snapshot does not exist', 'snapshot_path', '=', [ic%snapshot_path])
         return
      end if

      call rhyme_initial_condition_load_headers(ic, samr)

      samr%levels%nboxes = 0 ! It will be incremented by init_box procedure

      select case (ic%snapshot_type)
      case (icid%rhyme)
         call logger%log('Loading Rhyme snapshot')
         call rhyme_initial_condition_load_rhyme(ic, samr, logger)
      case default
         call logger%err('Unsupported snapshot format', &
                         'snapshot_type', '=', [ic%snapshot_type])
      end select

      samr%initialized = .true.
   end subroutine rhyme_initial_condition_load_snapshot
end submodule rhyme_ic_load_snapshot_smod
