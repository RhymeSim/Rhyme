submodule(rhyme_plotter) masked_max_array
contains
   pure module function rhyme_plotter_masked_max_1d_array_r8(arr, mask) result(maximum)
      implicit none

      real(kind=8), intent(in) :: arr(:)
      integer, intent(in) :: mask

      real(kind=8) :: maximum

      select case (mask)
      case (plid%gtr_eq_zero)
         maximum = maxval(arr, arr < 0d0)
      case (plid%gtr_zero)
         maximum = maxval(arr, arr <= 0d0)
      case (plid%less_eq_zero)
         maximum = maxval(arr, arr > 0d0)
      case (plid%less_zero)
         maximum = maxval(arr, arr >= 0d0)
      case (plid%no_mask)
         maximum = maxval(arr)
      case default
         maximum = maxval(arr)
      end select
   end function rhyme_plotter_masked_max_1d_array_r8

   pure module function rhyme_plotter_masked_max_2d_array_r8(arr, mask) result(maximum)
      implicit none

      real(kind=8), intent(in) :: arr(:, :)
      integer, intent(in) :: mask

      real(kind=8) :: maximum

      select case (mask)
      case (plid%gtr_eq_zero)
         maximum = maxval(arr, arr < 0d0)
      case (plid%gtr_zero)
         maximum = maxval(arr, arr <= 0d0)
      case (plid%less_eq_zero)
         maximum = maxval(arr, arr > 0d0)
      case (plid%less_zero)
         maximum = maxval(arr, arr >= 0d0)
      case (plid%no_mask)
         maximum = maxval(arr)
      case default
         maximum = maxval(arr)
      end select
   end function rhyme_plotter_masked_max_2d_array_r8

   pure module function rhyme_plotter_masked_max_3d_array_r8(arr, mask) result(maximum)
      implicit none

      real(kind=8), intent(in) :: arr(:, :, :)
      integer, intent(in) :: mask

      real(kind=8) :: maximum

      select case (mask)
      case (plid%gtr_eq_zero)
         maximum = maxval(arr, arr < 0d0)
      case (plid%gtr_zero)
         maximum = maxval(arr, arr <= 0d0)
      case (plid%less_eq_zero)
         maximum = maxval(arr, arr > 0d0)
      case (plid%less_zero)
         maximum = maxval(arr, arr >= 0d0)
      case (plid%no_mask)
         maximum = maxval(arr)
      case default
         maximum = maxval(arr)
      end select
   end function rhyme_plotter_masked_max_3d_array_r8
end submodule masked_max_array
