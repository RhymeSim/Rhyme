submodule(rhyme_chombo) output_clean_backups_smod
contains
   module subroutine rhyme_chombo_outpu_clean_backups(this, chombo, logger)
      implicit none

      class(chombo_output_t), intent(inout) :: this
      type(chombo_t), intent(in) :: chombo
      type(logger_t), intent(inout) :: logger

      character(len=1024) :: filename
      integer :: stat
      logical :: file_exists

      if (this%restart_backups(1) > 0) then
         call logger%log('Cleaning old restart backup!', 'iteration', '=', [this%restart_backups(1)])

         call rhyme_chombo_filename_generator( &
            chombo%prefix, chombo%nickname, this%restart_backups(1), filename)

         inquire (file=filename, exist=file_exists)

         if (file_exists) then
            call logger%log('Found the restart backup file!', '', 'Deleting...')
            open (unit=3456, iostat=stat, file=filename, status='old')
            if (stat == 0) then
               close (3456, status='delete')
               this%restart_backups(1) = -1
               call logger%log(trim(filename), 'has been deleted!')
            end if
         else
            call logger%warn('Restart backup file has not found!', 'path', ':', [trim(filename)])
         end if
      else
         call logger%log('No restart backup found!')
      end if
   end subroutine rhyme_chombo_outpu_clean_backups
end submodule output_clean_backups_smod
