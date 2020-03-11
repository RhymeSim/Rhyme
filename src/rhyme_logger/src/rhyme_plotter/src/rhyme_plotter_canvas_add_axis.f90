submodule(rhyme_plotter) canvas_add_axis_smod
contains
module subroutine rhyme_plotter_canvas_add_axis(canvas, axis, n_ticks, &
                                                minmax, scale, label, color)
   implicit none

   class(plotter_canvas_t), intent(inout) :: canvas
   integer, intent(in) :: axis, n_ticks
   real(kind=8), intent(in) :: minmax(2)
   integer, intent(in), optional :: scale
   character(len=*), intent(in), optional :: label, color

   integer :: ntick

   canvas%axes(axis)%is_on = .true.
   canvas%axes(axis)%n_ticks = min(n_ticks, max_nticks)
   canvas%axes(axis)%min = minmax(1)
   canvas%axes(axis)%max = minmax(2)

   if (present(color)) then
      canvas%axes(axis)%color = color
   else
      canvas%axes(axis)%color = ''
   end if

   if (present(scale)) then
      canvas%axes(axis)%scale = scale
   else
      canvas%axes(axis)%scale = plid%linear
   end if

   if (axis .eq. plid%left .or. axis .eq. plid%right) then
      canvas%axes(axis)%tick_width_px = real(canvas%y, kind=8) &
                                        /(canvas%axes(axis)%n_ticks - 1)
   else
      canvas%axes(axis)%tick_width_px = real(canvas%x, kind=8) &
                                        /(canvas%axes(axis)%n_ticks - 1)
   end if

   select case (canvas%axes(axis)%scale)
   case (plid%linear)
      canvas%axes(axis)%dx = (canvas%axes(axis)%max - canvas%axes(axis)%min) &
                             /(canvas%axes(axis)%n_ticks - 1)

      do ntick = 1, canvas%axes(axis)%n_ticks
         canvas%axes(axis)%ticks(ntick) = canvas%axes(axis)%min + canvas%axes(axis)%dx*(ntick - 1)
         write (canvas%axes(axis)%tick_labels(ntick), '(ES10.2)') canvas%axes(axis)%ticks(ntick)
      end do

   case (plid%log)
      canvas%axes(axis)%dx = 1d1**( &
                             log10(canvas%axes(axis)%max/canvas%axes(axis)%min) &
                             /(canvas%axes(axis)%n_ticks - 1) &
                             )

      do ntick = 1, canvas%axes(axis)%n_ticks
         canvas%axes(axis)%ticks(ntick) = canvas%axes(axis)%min*canvas%axes(axis)%dx**(ntick - 1)
         write (canvas%axes(axis)%tick_labels(ntick), '(ES10.2)') canvas%axes(axis)%ticks(ntick)
      end do

   case default
      return
   end select

   select case (axis)
   case (plid%left)
      call rhyme_plotter_canvas_add_vertical_axis(plid%left)
   case (plid%bottom)
      call rhyme_plotter_canvas_add_horizontal_axis(plid%bottom)
   case (plid%right)
      call rhyme_plotter_canvas_add_vertical_axis(plid%right)
   case (plid%top)
      call rhyme_plotter_canvas_add_horizontal_axis(plid%top)
   end select

contains

   subroutine rhyme_plotter_canvas_add_vertical_axis(axis)
      implicit none

      integer, intent(in) :: axis

      integer :: x, y, nbin
      integer :: axis_col, label_col
      integer :: ls, le ! label_start, label_end
      character(len=17, kind=ucs4) :: tick_char_clr, axis_char_clr
      character(len=1, kind=ucs4) :: tick_char, axis_char

      if (axis .eq. plid%left) then
         axis_col = 0
         label_col = -15
         ls = -12
         le = -3
         if (present(color)) then
            write (tick_char_clr, '(A12,A1,A4)') color, char(int(z'2524'), ucs4), colors%nc
         else
            tick_char_clr = char(int(z'2524'), ucs4)
         end if
         tick_char = char(int(z'2524'), ucs4)
      else if (axis .eq. plid%right) then
         axis_col = canvas%x + 1
         label_col = canvas%x + 16
         ls = canvas%x + 2
         le = canvas%x + 11
         if (present(color)) then
            write (tick_char_clr, '(A12,A1,A4)') color, char(int(z'251C'), ucs4), colors%nc
         else
            tick_char_clr = char(int(z'251C'), ucs4)
         end if
         tick_char = char(int(z'251C'), ucs4)
      else
         return
      end if

      if (present(color)) then
         write (axis_char_clr, '(A12,A1,A4)') color, char(int(z'2502'), ucs4), colors%nc
      else
         axis_char_clr = char(int(z'2502'), ucs4)
      end if

      axis_char = char(int(z'2502'), ucs4)

      canvas%grid(axis_col, 1:canvas%y, plid%bw) = axis_char
      canvas%grid(axis_col, 1:canvas%y, plid%clr) = axis_char_clr

      nbin = 0
      do y = 1, canvas%y
         if (y .eq. int(nbin*canvas%axes(axis)%tick_width_px) + 1 .or. y .eq. canvas%y) then
            nbin = nbin + 1

            do x = ls, le
               canvas%grid(x, canvas%y - y + 1, :) = canvas%axes(axis)%tick_labels(nbin) (1 + x - ls:1 + x - ls)
            end do

            canvas%grid(axis_col, canvas%y - y + 1, plid%bw) = tick_char
            canvas%grid(axis_col, canvas%y - y + 1, plid%clr) = tick_char_clr
         end if
      end do

      if (present(label)) then
         ls = max((canvas%y - len_trim(label))/2, 1)
         le = min(ls + len_trim(label) - 1, canvas%y)

         do y = ls, le
            canvas%grid(label_col, y, :) = label(y - ls + 1:y - ls + 1)
         end do
      end if
   end subroutine rhyme_plotter_canvas_add_vertical_axis

   subroutine rhyme_plotter_canvas_add_horizontal_axis(axis)
      implicit none

      integer, intent(in) :: axis

      integer :: x, xl, nbin
      integer :: axis_row, labels_row, label_row
      integer :: ls, le ! ls, le
      character(len=17, kind=ucs4) :: tick_char_clr, axis_char_clr
      character(len=1, kind=ucs4) :: tick_char, axis_char

      if (axis .eq. plid%bottom) then
         axis_row = canvas%y + 1
         labels_row = canvas%y + 2
         label_row = canvas%y + 4
         if (present(color)) then
            write (tick_char_clr, '(A12,A1,A4)') color, char(int(z'252C'), ucs4), colors%nc
         else
            tick_char_clr = char(int(z'252C'), ucs4)
         end if
         tick_char = char(int(z'252C'), ucs4)
      else if (axis .eq. plid%top) then
         axis_row = 0
         labels_row = -1
         label_row = -3
         if (present(color)) then
            write (tick_char_clr, '(A12,A1,A4)') color, char(int(z'2534'), ucs4), colors%nc
         else
            tick_char_clr = char(int(z'2534'), ucs4)
         end if
         tick_char = char(int(z'2534'), ucs4)
      else
         return
      end if

      if (present(color)) then
         write (axis_char_clr, '(A12,A1,A4)') color, char(int(z'2500'), ucs4), colors%nc
      else
         axis_char_clr = char(int(z'2500'), ucs4)
      end if
      axis_char = char(int(z'2500'), ucs4)

      canvas%grid(1:canvas%x, axis_row, plid%bw) = axis_char
      canvas%grid(1:canvas%x, axis_row, plid%clr) = axis_char_clr

      nbin = 0
      do x = 1, canvas%x
         if (x .eq. int(nbin*canvas%axes(axis)%tick_width_px) + 1 .or. x .eq. canvas%x) then
            nbin = nbin + 1

            canvas%grid(x, axis_row, plid%bw) = tick_char
            canvas%grid(x, axis_row, plid%clr) = tick_char_clr

            ls = max( &
                 int(x - len_trim(canvas%axes(axis)%tick_labels(nbin))/2) - 1, &
                 int(x - canvas%axes(axis)%tick_width_px/2) - 1, &
                 -offset_x)
            le = min( &
                 ls + len_trim(canvas%axes(axis)%tick_labels(nbin)), &
                 ls + int(canvas%axes(axis)%tick_width_px), &
                 canvas%x + offset_x)

            do xl = ls, le
               canvas%grid(xl, labels_row, :) = canvas%axes(axis)%tick_labels(nbin) (1 + xl - ls:1 + xl - ls)
            end do
         end if
      end do

      if (present(label)) then
         ls = max((canvas%x - len_trim(label))/2, 1)
         le = min(ls + len_trim(label) - 1, canvas%x)

         do xl = ls, le
            canvas%grid(xl, label_row, :) = label(1 + xl - ls:1 + xl - ls)
         end do
      end if
   end subroutine rhyme_plotter_canvas_add_horizontal_axis
end subroutine rhyme_plotter_canvas_add_axis
end submodule canvas_add_axis_smod
