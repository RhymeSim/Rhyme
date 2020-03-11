submodule(rhyme_plotter) canvas_plot_smod
contains
module subroutine rhyme_plotter_canvas_plot(canvas, output, colored)
   use iso_fortran_env

   implicit none

   class(plotter_canvas_t), intent(inout) :: canvas
   integer, intent(in), optional :: output
   logical, intent(in), optional :: colored

   integer :: i, j, row_len, out, canvas_type
   character(len=2048, kind=ucs4) :: row

   if (present(output)) then
      out = output
   else
      out = output_unit
   end if

   if (present(colored)) then
      if (colored) then
         canvas_type = plid%clr
      else
         canvas_type = plid%bw
      end if
   else
      canvas_type = plid%clr
   end if

   open (out, encoding='UTF-8')

   do j = canvas%lbound_y, canvas%ubound_y
      row = ''
      row_len = 1

      do i = canvas%lbound_x, canvas%ubound_x
         if (len_trim(canvas%grid(i, j, canvas_type)) .eq. 0) then
            row = row(1:row_len)//char(int(z'0020'), ucs4)
            row_len = row_len + 1
         else
            row = row(1:row_len)//trim(canvas%grid(i, j, canvas_type))
            row_len = row_len + len_trim(canvas%grid(i, j, canvas_type))
         end if
      end do

      write (out, *) trim(row)
   end do
end subroutine rhyme_plotter_canvas_plot
end submodule canvas_plot_smod
