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

      if (outputs%every > 0 .and. mod(outputs%every, iteration) == 0) then
         be_saved = .true.
         return
      end if

      if (.not. allocated(outputs%times) .or. size(outputs%times) < 1) then
         return
      end if

      if (time < outputs%times(1)) then
         be_saved = .false.
         return
      end if

      i = 1
      do while (i + 1 <= size(outputs%times) .and. time > outputs%times(i + 1))
         i = i + 1
      end do

      if (outputs%saved(i)) then
         be_saved = .false.
         return
      else
         outputs%saved(i) = .true.
         be_saved = .true.
         return
      end if
   end function rhyme_chombo_output_should_be_saved
end submodule output_should_be_saved_smod
