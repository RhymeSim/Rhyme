submodule(rhyme_plotter) canvas_draw_image_smod
contains
module subroutine rhyme_plotter_canvas_draw_image( &
   canvas, image, values, xaxis_op, yaxis_op, colorscheme_op, &
   cs_min_op, cs_max_op, cs_scale_op)
   implicit none

   class(plotter_canvas_t), intent(inout) :: canvas
   type(plotter_image_t), intent(in) :: image
   real(kind=8), intent(in) :: values(:, :)
   integer, intent(in), optional :: xaxis_op, yaxis_op
   type(colorscheme_t), intent(in), optional :: colorscheme_op
   real(kind=8), intent(in), optional :: cs_min_op, cs_max_op
   integer, intent(in), optional :: cs_scale_op

   integer :: i, j, k, xaxis, yaxis
   type(colorscheme_t) :: colorscheme
   real(kind=8) :: cs_min, cs_max
   integer :: cs_scale
   integer :: xpx, ypx
   real(kind=8) :: x, y
   type(color_t) :: char_colors(-1:0) ! We separate a character into bottom and top

   if (present(xaxis_op)) then
      xaxis = xaxis_op
   else
      xaxis = plid%bottom
   end if

   if (present(yaxis_op)) then
      yaxis = yaxis_op
   else
      yaxis = plid%left
   end if

   if (present(colorscheme_op)) then
      colorscheme = colorscheme_op
   else
      colorscheme = colorschemes(csid%magma_grey)
   end if

   if (present(cs_min_op)) then
      cs_min = cs_min_op
   else
      cs_min = minval(values)
   end if

   if (present(cs_max_op)) then
      cs_max = cs_max_op
   else
      cs_max = maxval(values)
   end if

   if (present(cs_scale_op)) then
      cs_scale = cs_scale_op
   else
      cs_scale = plid%linear
   end if

   do i = 1, min(canvas%x, 72)
   do j = 1, canvas%y
      do k = -1, 0
         char_colors(k) = colorscheme%pallet(csid%unknown)

         x = pixel_to_point(i, canvas%axes(xaxis), canvas%x)
         y = pixel_to_point(2*j + k, canvas%axes(yaxis), 2*canvas%y)

         if (x < image%x%min .or. x > image%x%max) cycle
         if (y < image%y%min .or. y > image%y%max) cycle

         xpx = point_to_pixel(x, image%x%min, image%x%max, image%x%scale, image%x%resolution)
         ypx = point_to_pixel(y, image%y%min, image%y%max, image%y%scale, image%y%resolution)

         if (xpx < 1 .or. xpx > image%x%resolution) cycle
         if (ypx < 1 .or. ypx > image%y%resolution) cycle

         char_colors(k) = pick_color(values(xpx, ypx))
      end do
      write (canvas%grid(i, j, plid%clr), '(A11,A11,A1,A4)') &
         char_colors(-1)%bg, char_colors(0)%fg, char(int(z'2584'), ucs4), tc%nc
   end do
   end do

   call canvas%add_corner(xaxis, yaxis)

contains
   ! TODO: move these functions to plotter module
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
      pixel, axis, npixels) result(point)
      implicit none

      integer, intent(in) :: pixel
      type(plotter_canvas_axis_t), intent(in) :: axis
      integer, intent(in) :: npixels

      select case (axis%scale)
      case (plid%linear)
         point = (real(pixel, kind=8) - .5)/npixels*(axis%max - axis%min) + axis%min
      case (plid%log)
         point = log10(real(pixel, kind=8) - .5)/log10(real(npixels, kind=8)) &
                 *(axis%max - axis%min) + axis%min
      case default
         point = (real(pixel, kind=8) - .5)/npixels*(axis%max - axis%min) + axis%min
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
end subroutine rhyme_plotter_canvas_draw_image
end submodule canvas_draw_image_smod
