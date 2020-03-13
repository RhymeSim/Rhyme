submodule(rhyme_plotter) canvas_draw_histogram_smod
contains
module subroutine rhyme_plotter_canvas_draw_histogram( &
   canvas, nbins, centers, heights, xaxis, yaxis, color)
   implicit none

   class(plotter_canvas_t), intent(inout) :: canvas
   integer, intent(in) :: nbins
   real(kind=8), dimension(:), intent(in) :: centers, heights
   integer, intent(in), optional :: xaxis, yaxis
   character(len=*), intent(in), optional :: color

   integer :: xa, ya
   integer :: xpx, ypx
   integer :: i, j, tip

   if (present(xaxis)) then
      xa = xaxis
   else
      xa = plid%bottom
   end if

   if (present(yaxis)) then
      ya = yaxis
   else
      ya = plid%left
   end if

   do i = 1, nbins
      if (heights(i) < canvas%axes(ya)%min) cycle
      if (centers(i) < canvas%axes(xa)%min .or. centers(i) > canvas%axes(xa)%max) cycle

      xpx = pixel_position(centers(i), canvas, xa)
      ypx = max(pixel_position(centers(i), canvas, xa), canvas%y)

      if (.not. ypx > 0) cycle

      tip = canvas%y - ypx + 1

      do j = canvas%y, tip + 1, -1
      end do
   end do

contains
   integer pure function pixel_position(pos, c, axis) result(px)
      implicit none

      real(kind=8), intent(in) :: pos
      class(plotter_canvas_t), intent(in) :: c
      integer, intent(in) :: axis

      select case (c%axes(axis)%scale)
      case (plid%linear)
         px = floor( &
              (pos - c%axes(axis)%min) &
              /c%axes(axis)%dx &
              *c%axes(axis)%tick_width_px &
              ) + 1
      case (plid%log)
         if (pos > 0 .and. c%axes(axis)%min*c%axes(axis)%min > 0) then
            px = floor( &
                 pos &
                 )
         else
            px = 0
         end if
      case default
         px = 0
      end select
   end function pixel_position
end subroutine rhyme_plotter_canvas_draw_histogram
end submodule canvas_draw_histogram_smod
