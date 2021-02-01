submodule(rhyme_plotter) two_d_histogram_smod
   implicit none

contains

   pure module function rhyme_plotter_two_d_histogram( &
      x, y, xbins, ybins, xscale, yscale, xminmax, yminmax, &
      normalized, mask) result(hist2d)
      implicit none

      real(kind=8), intent(in) :: x(:), y(:)
      integer, intent(in) :: xbins, ybins, xscale, yscale
      real(kind=8), intent(in), optional :: xminmax(2), yminmax(2)
      logical, intent(in), optional :: normalized
      integer, intent(in), optional :: mask(2)

      type(plotter_2d_histogram_t) :: hist2d

      integer :: i, xpx, ypx, total_number, msk(2)

      total_number = 0

      if (size(x) /= size(y)) then
         hist2d%counts = 0d0
         return
      end if

      if (present(xminmax)) then
         hist2d%x%min = xminmax(1)
         hist2d%x%max = xminmax(2)
      else
         hist2d%x%min = rhyme_plotter_masked_min(x, msk(1))
         hist2d%x%max = rhyme_plotter_masked_max(x, msk(1))
      end if

      if (present(yminmax)) then
         hist2d%y%min = yminmax(1)
         hist2d%y%max = yminmax(2)
      else
         hist2d%y%min = rhyme_plotter_masked_min(y, msk(2))
         hist2d%y%max = rhyme_plotter_masked_max(y, msk(2))
      end if

      if (present(mask)) then
         msk = mask
      else
         msk = plid%no_mask
      end if

      hist2d%x%nbins = min(xbins, max_nbins)
      hist2d%x%scale = xscale

      hist2d%y%nbins = min(ybins, max_nbins)
      hist2d%y%scale = yscale

      hist2d%counts = 0d0

      hist2d%x%n_masked = 0
      hist2d%y%n_masked = 0

      do i = 1, size(x)
         if (x(i) < hist2d%x%min .or. x(i) > hist2d%x%max) cycle
         if (rhyme_plotter_is_masked(x(i), msk(1))) then
            hist2d%x%n_masked = hist2d%x%n_masked + 1
            cycle
         end if

         if (y(i) < hist2d%y%min .or. y(i) > hist2d%y%max) cycle
         if (rhyme_plotter_is_masked(y(i), msk(2))) then
            hist2d%y%n_masked = hist2d%y%n_masked + 1
            cycle
         end if

         total_number = total_number + 1

         xpx = get_pixel(x(i), hist2d%x, xbins)
         ypx = get_pixel(y(i), hist2d%y, ybins)

         hist2d%counts(xpx, ypx) = hist2d%counts(xpx, ypx) + 1
      end do

      if (.not. present(normalized) .or. (present(normalized) .and. normalized .eqv. .true.)) then
         hist2d%counts = hist2d%counts/real(total_number)
      end if
   contains
      pure integer function get_pixel(point, axis, nbins) result(px)
         implicit none

         real(kind=8), intent(in) :: point
         type(plotter_histogram_axis_t), intent(in) :: axis
         integer, intent(in) :: nbins

         select case (axis%scale)
         case (plid%linear)
            px = floor((point - axis%min)/(axis%max - axis%min)*nbins) + 1
         case (plid%log)
            px = floor(log10(point/axis%min)/log10(axis%max/axis%min)*nbins) + 1
         case default
            px = 0
         end select
      end function get_pixel
   end function rhyme_plotter_two_d_histogram
end submodule two_d_histogram_smod
