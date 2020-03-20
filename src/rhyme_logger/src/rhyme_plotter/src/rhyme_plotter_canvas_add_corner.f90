submodule(rhyme_plotter) canvas_add_corner_smod
contains
module subroutine rhyme_plotter_canvas_add_corner( &
   canvas, xaxis, yaxis)

   implicit none

   integer, intent(in) :: xaxis, yaxis
   class(plotter_canvas_t), intent(inout) :: canvas

   character(len=12, kind=ucs4) :: clr
   character(len=4, kind=ucs4) :: nc

   ! corners
   if (len_trim(canvas%axes(xaxis)%color) > 0) then
      clr = canvas%axes(xaxis)%color
      nc = tc%nc
   else
      clr = ''
      nc = ''
   end if

   if (xaxis .eq. plid%bottom) then
      if (yaxis .eq. plid%left) then
         canvas%grid(0, canvas%y + 1, plid%clr) = trim(clr)//char(int(z'2514'), ucs4)//trim(nc)
         canvas%grid(0, canvas%y + 1, plid%bw) = char(int(z'2514'), ucs4)
      else if (yaxis .eq. plid%right) then
         canvas%grid(canvas%x + 1, canvas%y + 1, plid%clr) = trim(clr)//char(int(z'2518'), ucs4)//trim(nc)
         canvas%grid(canvas%x + 1, canvas%y + 1, plid%bw) = char(int(z'2518'), ucs4)
      end if
   else if (xaxis .eq. plid%top) then
      if (yaxis .eq. plid%left) then
         canvas%grid(0, 0, plid%clr) = trim(clr)//char(int(z'250C'), ucs4)//trim(nc)
         canvas%grid(0, 0, plid%bw) = char(int(z'250C'), ucs4)
      else if (yaxis .eq. plid%right) then
         canvas%grid(canvas%x + 1, 0, plid%clr) = trim(clr)//char(int(z'2510'), ucs4)//trim(nc)
         canvas%grid(canvas%x + 1, 0, plid%bw) = char(int(z'2510'), ucs4)
      end if
   end if

end subroutine rhyme_plotter_canvas_add_corner
end submodule canvas_add_corner_smod
