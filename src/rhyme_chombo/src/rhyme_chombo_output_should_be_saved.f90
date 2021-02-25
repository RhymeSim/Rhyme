submodule(rhyme_chombo) output_should_be_saved_smod
contains
   module function rhyme_chombo_output_should_be_saved( &
      outputs, iteration, time) result(be_saved)
      class(chombo_output_t), intent(inout) :: outputs
      integer, intent(in) :: iteration
      real(kind=8), intent(in) :: time

      logical :: be_saved

      integer :: i

      be_saved = .false.

      if (outputs%every > 0 .and. mod(iteration, outputs%every) == 0) then
         be_saved = .true.
         return
      end if

      if (allocated(outputs%times) .and. size(outputs%times) >= 1) then
         i = 1
         do while (i + 1 <= size(outputs%times) .and. time > outputs%times(i + 1))
            i = i + 1
         end do

         if (time > 0d0 .and. .not. outputs%saved(i)) then
            outputs%saved(i) = .true.
            be_saved = .true.
            return
         end if
      end if

      if (outputs%restart_backup_every > 0 .and. mod(iteration, outputs%restart_backup_every) == 0) then
         be_saved = .true.
         outputs%restart_backups(1) = outputs%restart_backups(2)
         outputs%restart_backups(2) = iteration
         return
      end if
   end function rhyme_chombo_output_should_be_saved
end submodule output_should_be_saved_smod
