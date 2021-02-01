submodule(rhyme_plotter) is_masked_scalar_smod
contains
   pure module function rhyme_plotter_is_masked_scalar_r8(x, mask) result(masked)
      implicit none

      real(kind=8), intent(in) :: x
      integer, intent(in) :: mask
      logical :: masked

      masked = .false.

      select case (mask)
      case (plid%gtr_eq_zero)
         if (x >= 0d0) then
            masked = .true.
         end if
      case (plid%gtr_zero)
         if (x > 0d0) then
            masked = .true.
         end if
      case (plid%less_eq_zero)
         if (x <= 0d0) then
            masked = .true.
         end if
      case (plid%less_zero)
         if (x < 0d0) then
            masked = .true.
         end if
      case (plid%no_mask)
         masked = .false.
      case default
         masked = .false.
      end select
   end function rhyme_plotter_is_masked_scalar_r8
end submodule is_masked_scalar_smod
