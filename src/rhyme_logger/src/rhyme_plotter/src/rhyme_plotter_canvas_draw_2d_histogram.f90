submodule(rhyme_plotter) canvas_draw_2d_histogram_smod
contains
module subroutine rhyme_plotter_canvs_draw_2d_histogram( &
   canvas, hist, xaxis, yaxis, colorscheme_op, &
   cs_min_op, cs_max_op, cs_scale_op)
   implicit none

   class(plotter_canvas_t), intent(inout) :: canvas
   type(plotter_2d_histogram_t), intent(in) :: hist
   integer, intent(in), optional :: xaxis, yaxis
   type(colorscheme_t), intent(in), optional :: colorscheme_op
   real(kind=8), intent(in), optional :: cs_min_op, cs_max_op
   integer, intent(in), optional :: cs_scale_op

   integer :: i, j, k
   integer :: hist_x, hist_y
   type(color_t) :: char_colors(-1:0) ! We separate a character into bottom and top
   type(plotter_canvas_axis_t) :: xax, yax
   real(kind=8), dimension(max_nbins, max_nbins) :: counts
   real(kind=8) :: point_x, point_y
   type(colorscheme_t) :: colorscheme
   real(kind=8) :: cs_min, cs_max
   integer :: cs_scale

   counts = hist%counts

   if (present(xaxis)) then
      xax = canvas%axes(xaxis)
   else
      xax = canvas%axes(plid%bottom)
   end if

   if (present(yaxis)) then
      yax = canvas%axes(yaxis)
   else
      yax = canvas%axes(plid%left)
   end if

   if (present(colorscheme_op)) then
      colorscheme = colorscheme_op
   else
      colorscheme = colorschemes(csid%magma_grey)
   end if

   if (present(cs_min_op)) then
      cs_min = cs_min_op
   else
      cs_min = minval(hist%counts)
   end if

   if (present(cs_max_op)) then
      cs_max = cs_max_op
   else
      cs_max = maxval(hist%counts)
   end if

   if (present(cs_scale_op)) then
      cs_scale = cs_scale_op
   else
      cs_scale = plid%linear
   end if

   do i = 1, canvas%x
   do j = 1, canvas%y
      do k = -1, 0 ! We divide each character into two regions (top and bottom)
         ! TODO: add csid%unknown color
         char_colors(k) = colorscheme%pallet(csid%low_end)

         point_x = pixel_to_point(i, xax%min, xax%max, xax%scale, canvas%x)
         point_y = pixel_to_point(2*j + k, yax%min, yax%max, yax%scale, 2*canvas%y)

         if (point_x < hist%x%min .or. point_x > hist%x%max) cycle
         if (point_y < hist%y%min .or. point_y > hist%y%max) cycle

         hist_x = point_to_pixel(point_x, hist%x%min, hist%x%max, hist%x%scale, hist%x%nbins)
         hist_y = point_to_pixel(point_y, hist%y%min, hist%y%max, hist%y%scale, hist%y%nbins)

         if (hist_x < 1 .or. hist_x > hist%x%nbins) cycle
         if (hist_y < 1 .or. hist_y > hist%y%nbins) cycle

         char_colors(k) = pick_color(hist%counts(hist_x, hist_y))
      end do

      write (canvas%grid(i, j, plid%clr), '(A11,A11,A1,A4)') &
         char_colors(-1)%bg, char_colors(0)%fg, char(int(z'2584'), ucs4), tc%nc
   end do
   end do

contains
   type(color_t) function pick_color(val) result(color)
      implicit none

      real(kind=8), intent(in) :: val

      integer :: idx

      select case (cs_scale)
      case (plid%linear)
         idx = floor((val - cs_min)/(cs_max - cs_min)*colorscheme%n) + 1
      case (plid%log)
         idx = floor(log10(val/cs_min)/log10(cs_max/cs_min)*colorscheme%n) + 1
      case default
         idx = floor((val - cs_min)/(cs_max - cs_min)*colorscheme%n) + 1
      end select

      if (idx < 1) idx = csid%low_end
      if (idx > colorscheme%n) idx = csid%high_end

      color = colorscheme%pallet(idx)
   end function pick_color

   real(kind=8) function pixel_to_point( &
      pixel, range_min, range_max, range_scale, npixels) result(point)
      implicit none

      integer, intent(in) :: pixel
      real(kind=8), intent(in) :: range_min, range_max
      integer, intent(in) :: npixels, range_scale

      select case (range_scale)
      case (plid%linear)
         point = (real(pixel, kind=8) - .5)/npixels*(range_max - range_min) + range_min
      case (plid%log)
         point = log10(real(pixel, kind=8) - .5)/log10(real(npixels, kind=8)) &
                 *(range_max - range_min) + range_min
      case default
         point = (real(pixel, kind=8) - .5)/npixels*(range_max - range_min) + range_min
      end select
   end function pixel_to_point

   integer function point_to_pixel( &
      point, range_min, range_max, range_scale, npixels) result(pixel)
      implicit none

      real(kind=8), intent(in) :: point, range_min, range_max
      integer, intent(in) :: range_scale, npixels

      select case (range_scale)
      case (plid%linear)
         pixel = floor((point - range_min)/(range_max - range_min)*npixels) + 1
      case (plid%log)
         pixel = floor(log10(point/range_min)/log10(range_max/range_min)*npixels) + 1
      case default
         pixel = floor((point - range_min)/(range_max - range_min)*npixels) + 1
      end select
   end function point_to_pixel
end subroutine rhyme_plotter_canvs_draw_2d_histogram
end submodule canvas_draw_2d_histogram_smod
