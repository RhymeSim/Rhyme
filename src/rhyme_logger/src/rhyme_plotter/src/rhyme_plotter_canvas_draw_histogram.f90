submodule(rhyme_plotter) canvas_draw_histogram_smod
contains
module subroutine rhyme_plotter_canvas_draw_histogram( &
   canvas, hist, xaxis, yaxis, color)
   implicit none

   class(plotter_canvas_t), intent(inout) :: canvas
   type(plotter_histogram_t), intent(in) :: hist
   integer, intent(in), optional :: xaxis, yaxis
   character(len=*), intent(in), optional :: color

   integer :: xa, ya
   integer :: xpx
   real(kind=8) :: ypx
   integer :: i, j, tip_px
   real(kind=8), dimension(max_nbins) :: counts, centers
   character(len=1, kind=ucs4) :: tip_char_bw
   character(len=17, kind=ucs4) :: tip_char_clr
   character(len=12, kind=ucs4) :: clr
   character(len=4, kind=ucs4) :: nc

   centers = hist%bin_centers
   counts = hist%counts

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

   do i = 1, hist%nbins
      if (counts(i) < canvas%axes(ya)%min) cycle
      if (centers(i) < canvas%axes(xa)%min .or. centers(i) > canvas%axes(xa)%max) cycle

      xpx = floor(pixel_position(centers(i), canvas, xa, canvas%x)) + 1
      ypx = min(pixel_position(counts(i), canvas, ya, canvas%y), real(canvas%y, kind=8))

      if (ypx > 0) then
         tip_px = canvas%y - floor(ypx)

         do j = canvas%y, tip_px + 1, -1
            if (present(color)) then
               write (canvas%grid(xpx, j, plid%clr), csid%char_color_fmt) &
                  color, char(int(z'2588'), ucs4), tc%nc
            else
               canvas%grid(xpx, j, plid%clr) = char(int(z'2588'), ucs4)
            end if
            canvas%grid(xpx, j, plid%bw) = char(int(z'2588'), ucs4)
         end do

         tip_char_bw = get_tip_char(ypx)

         if (present(color)) then
            write (tip_char_clr, csid%char_color_fmt) color, tip_char_bw, tc%nc
         else
            tip_char_clr = tip_char_bw
         end if

         canvas%grid(xpx, tip_px, plid%clr) = tip_char_clr
         canvas%grid(xpx, tip_px, plid%bw) = tip_char_bw
      end if
   end do

   ! corners
   if (len_trim(canvas%axes(xa)%color) > 0) then
      clr = canvas%axes(xa)%color
      nc = tc%nc
   else
      clr = ''
      nc = ''
   end if

   if (xa .eq. plid%bottom) then
      if (ya .eq. plid%left) then
         canvas%grid(0, canvas%y + 1, plid%clr) = trim(clr)//char(int(z'2514'), ucs4)//trim(nc)
         canvas%grid(0, canvas%y + 1, plid%bw) = char(int(z'2514'), ucs4)
      else if (ya .eq. plid%right) then
         canvas%grid(canvas%x + 1, canvas%y + 1, plid%clr) = trim(clr)//char(int(z'2518'), ucs4)//trim(nc)
         canvas%grid(canvas%x + 1, canvas%y + 1, plid%bw) = char(int(z'2518'), ucs4)
      end if
   else if (xa .eq. plid%top) then
      if (ya .eq. plid%left) then
         canvas%grid(0, 0, plid%clr) = trim(clr)//char(int(z'250C'), ucs4)//trim(nc)
         canvas%grid(0, 0, plid%bw) = char(int(z'250C'), ucs4)
      else if (ya .eq. plid%right) then
         canvas%grid(canvas%x + 1, 0, plid%clr) = trim(clr)//char(int(z'2510'), ucs4)//trim(nc)
         canvas%grid(canvas%x + 1, 0, plid%bw) = char(int(z'2510'), ucs4)
      end if
   end if

contains
   real(kind=8) pure function pixel_position(pos, cnvs, axis, length) result(px)
      implicit none

      real(kind=8), intent(in) :: pos
      class(plotter_canvas_t), intent(in) :: cnvs
      integer, intent(in) :: axis, length

      select case (cnvs%axes(axis)%scale)
      case (plid%linear)
         px = (pos - cnvs%axes(axis)%min) &
              /(cnvs%axes(axis)%max - cnvs%axes(axis)%min) &
              *length
      case (plid%log)
         if (pos > 0 .and. cnvs%axes(axis)%min*cnvs%axes(axis)%max > 0) then
            px = log10(pos/cnvs%axes(axis)%min) &
                 /log10(cnvs%axes(axis)%max/cnvs%axes(axis)%min) &
                 *length
         else
            px = 0
         end if
      case default
         px = 0
      end select
   end function pixel_position

   character(len=1, kind=ucs4) pure function get_tip_char(h) result(tip_char)
      implicit none

      real(kind=8), intent(in) :: h

      real(kind=8) :: diff

      diff = h - floor(h)

      if (diff < .125d0) then
         tip_char = char(int(z'2581'), ucs4)
      else if (diff < .250d0) then
         tip_char = char(int(z'2582'), ucs4)
      else if (diff < .375d0) then
         tip_char = char(int(z'2583'), ucs4)
      else if (diff < .500d0) then
         tip_char = char(int(z'2584'), ucs4)
      else if (diff < .620d0) then
         tip_char = char(int(z'2585'), ucs4)
      else if (diff < .750d0) then
         tip_char = char(int(z'2586'), ucs4)
      else if (diff < .875d0) then
         tip_char = char(int(z'2587'), ucs4)
      else
         tip_char = char(int(z'2588'), ucs4)
      end if
   end function get_tip_char
end subroutine rhyme_plotter_canvas_draw_histogram
end submodule canvas_draw_histogram_smod
