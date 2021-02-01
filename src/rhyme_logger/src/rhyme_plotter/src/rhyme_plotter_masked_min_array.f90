submodule(rhyme_plotter) masked_min_array
contains
   pure module function rhyme_plotter_masked_min_1d_array_r8(arr, mask) result(minimum)
      implicit none

      real(kind=8), intent(in) :: arr(:)
      integer, intent(in) :: mask

      real(kind=8) :: minimum

      select case (mask)
      case (plid%gtr_eq_zero)
         minimum = minval(arr, arr < 0d0)
      case (plid%gtr_zero)
         minimum = minval(arr, arr <= 0d0)
      case (plid%less_eq_zero)
         minimum = minval(arr, arr > 0d0)
      case (plid%less_zero)
         minimum = minval(arr, arr >= 0d0)
      case (plid%no_mask)
         minimum = minval(arr)
      case default
         minimum = minval(arr)
      end select
   end function rhyme_plotter_masked_min_1d_array_r8

   pure module function rhyme_plotter_masked_min_2d_array_r8(arr, mask) result(minimum)
      implicit none

      real(kind=8), intent(in) :: arr(:, :)
      integer, intent(in) :: mask

      real(kind=8) :: minimum

      select case (mask)
      case (plid%gtr_eq_zero)
         minimum = minval(arr, arr < 0d0)
      case (plid%gtr_zero)
         minimum = minval(arr, arr <= 0d0)
      case (plid%less_eq_zero)
         minimum = minval(arr, arr > 0d0)
      case (plid%less_zero)
         minimum = minval(arr, arr >= 0d0)
      case (plid%no_mask)
         minimum = minval(arr)
      case default
         minimum = minval(arr)
      end select
   end function rhyme_plotter_masked_min_2d_array_r8

   pure module function rhyme_plotter_masked_min_3d_array_r8(arr, mask) result(minimum)
      implicit none

      real(kind=8), intent(in) :: arr(:, :, :)
      integer, intent(in) :: mask

      real(kind=8) :: minimum

      select case (mask)
      case (plid%gtr_eq_zero)
         minimum = minval(arr, arr < 0d0)
      case (plid%gtr_zero)
         minimum = minval(arr, arr <= 0d0)
      case (plid%less_eq_zero)
         minimum = minval(arr, arr > 0d0)
      case (plid%less_zero)
         minimum = minval(arr, arr >= 0d0)
      case (plid%no_mask)
         minimum = minval(arr)
      case default
         minimum = minval(arr)
      end select
   end function rhyme_plotter_masked_min_3d_array_r8
end submodule masked_min_array
