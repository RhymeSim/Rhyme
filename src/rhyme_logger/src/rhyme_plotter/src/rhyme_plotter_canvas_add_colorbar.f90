submodule(rhyme_plotter) canvas_add_colorbar_smod
contains
   module subroutine rhyme_plotter_canvas_add_colorbar( &
      canvas, colorscheme, cs_min, cs_max, cs_scale, location_op, nticks_op)
      implicit none

      class(plotter_canvas_t), intent(inout) :: canvas
      type(colorscheme_t), intent(in) :: colorscheme
      real(kind=8), intent(in) :: cs_min, cs_max
      integer, intent(in) :: cs_scale
      integer, intent(in), optional :: location_op, nticks_op

      integer :: location, nticks

      if (present(location_op)) then
         location = location_op
      else
         location = plid%right
      end if

      if (present(nticks_op)) then
         nticks = nticks_op ! 2 for minimum and maximum
      else
         nticks = 5
      end if

      select case (location)
      case (plid%right)
         call add_vertical_colorbar( &
            canvas%x + 3, canvas%y, min(canvas%y - 4, (colorscheme%n + 1)/2), plid%right)
      case (plid%left)
         call add_vertical_colorbar( &
            -2, canvas%y, min(canvas%y - 4, (colorscheme%n + 1)/2), plid%left)
      end select
   contains
      subroutine add_vertical_colorbar(i, container, cb_length, side)
         implicit none

         integer, intent(in) :: i, container, cb_length
         integer, intent(in) :: side

         integer :: j, k, l
         integer :: offset
         integer :: c_fg, c_bg
         character(len=64) :: char_color, tick_label
         integer :: ticks(32)
         real(kind=8) :: dx, dxlog

         offset = (container - cb_length - 4)/2 + 1

         ! Unknonw
         j = canvas%y - offset

         select case (side)
         case (plid%right)
            canvas%grid(i + 2:i + 8, j, plid%clr) = ['U', 'n', 'k', 'n', 'o', 'w', 'n']
         case (plid%left)
            canvas%grid(i - 8:i - 2, j, plid%clr) = ['U', 'n', 'k', 'n', 'o', 'w', 'n']
         end select

         write (canvas%grid(i, j, plid%clr), '(A11,A1,A4)') &
            colorscheme%pallet(csid%unknown)%fg, char(int(z'25CF'), ucs4), tc%nc

         ! Low-end
         j = canvas%y - offset - 1

         write (canvas%grid(i, j, plid%clr), '(A11,A1,A4)') &
            colorscheme%pallet(csid%low_end)%fg, char(int(z'25CF'), ucs4), tc%nc

         select case (side)
         case (plid%right)
            canvas%grid(i + 2:i + 8, j, plid%clr) = ['L', 'o', 'w', '-', 'e', 'n', 'd']
         case (plid%left)
            canvas%grid(i - 8:i - 2, j, plid%clr) = ['L', 'o', 'w', '-', 'e', 'n', 'd']
         end select

         ! Colorbar
         do j = canvas%y - offset - 3, canvas%y - offset - 3 - cb_length, -1
            k = (container - offset - 3) - j + 1

            c_fg = floor(dble(2*k - 1 - .5)/(2*cb_length)*colorscheme%n) + 1

            if (c_fg >= 1 .and. c_fg <= colorscheme%n) then
               write (char_color, '(A11)') colorscheme%pallet(c_fg)%fg

               c_bg = floor(dble(2*k - .5)/(2*cb_length)*colorscheme%n) + 1

               if (c_bg >= 1 .and. c_bg <= colorscheme%n) then
                  write (canvas%grid(i, j, plid%clr), '(A11,A11,A1,A4)') &
                     char_color, colorscheme%pallet(c_bg)%bg, char(int(z'2584'), ucs4), tc%nc
               else
                  write (canvas%grid(i, j, plid%clr), '(A11,A1,A4)') &
                     char_color, char(int(z'2584'), ucs4), tc%nc
               end if

               select case (side)
               case (plid%right)
                  canvas%grid(i + 1, j, plid%clr) = char(int(z'2502'), ucs4)
               case (plid%left)
                  canvas%grid(i - 1, j, plid%clr) = char(int(z'2502'), ucs4)
               end select

            end if
         end do

         ! Minimum
         j = container - offset - 3
         write (tick_label, '(ES10.2)') cs_min

         select case (side)
         case (plid%right)
            canvas%grid(i + 1, j, plid%clr) = char(int(z'2514'), ucs4)
            canvas%grid(i + 2:i + 11, j, plid%clr) = [(tick_label(l:l), l=1, 10)]
         case (plid%left)
            canvas%grid(i - 1, j, plid%clr) = char(int(z'2518'), ucs4)
            canvas%grid(i - 12:i - 3, j, plid%clr) = [(tick_label(l:l), l=1, 10)]
         end select

         ! Ticks
         dx = dble(cb_length)/(nticks - 1)
         dxlog = 1d1**(log10(cs_max/cs_min)/(nticks - 1))
         ticks(2:nticks - 1) = [(floor(l*dx + .5) + 1, l=1, nticks - 1)]

         do j = 2, nticks - 1
            k = (container - offset - 3) - ticks(j) + 1

            select case (cs_scale)
            case (plid%linear)
               write (tick_label, '(ES10.2)') dble(ticks(j) - .5)/cb_length*(cs_max - cs_min) + cs_min
            case (plid%log)
               write (tick_label, '(ES10.2)') cs_min*dxlog**(j - 1)
            case default
               write (tick_label, '(ES10.2)') dble(ticks(j) - .5)/cb_length*(cs_max - cs_min) + cs_min
            end select

            select case (side)
            case (plid%right)
               canvas%grid(i + 1, k, plid%clr) = char(int(z'251C'), ucs4)
               canvas%grid(i + 2:i + 11, k, plid%clr) = [(tick_label(l:l), l=1, 10)]
            case (plid%left)
               canvas%grid(i - 1, k, plid%clr) = char(int(z'2524'), ucs4)
               canvas%grid(i - 12:i - 3, k, plid%clr) = [(tick_label(l:l), l=1, 10)]
            end select
         end do

         ! Maximum
         j = container - offset - 3 - cb_length + 1
         write (tick_label, '(ES10.2)') cs_max

         select case (side)
         case (plid%right)
            canvas%grid(i + 1, j, plid%clr) = char(int(z'250C'), ucs4)
            canvas%grid(i + 2:i + 11, j, plid%clr) = [(tick_label(l:l), l=1, 10)]
         case (plid%left)
            canvas%grid(i - 1, j, plid%clr) = char(int(z'2510'), ucs4)
            canvas%grid(i - 12:i - 3, j, plid%clr) = [(tick_label(l:l), l=1, 10)]
         end select

         ! High-end
         j = canvas%y - offset - 3 - cb_length - 2
         write (canvas%grid(i, j, plid%clr), '(A11,A1,A4)') &
            colorscheme%pallet(csid%high_end)%fg, char(int(z'25CF'), ucs4), tc%nc

         select case (side)
         case (plid%right)
            canvas%grid(i + 2:i + 9, j, plid%clr) = ['H', 'i', 'g', 'h', '-', 'e', 'n', 'd']
         case (plid%left)
            canvas%grid(i - 9:i - 2, j, plid%clr) = ['H', 'i', 'g', 'h', '-', 'e', 'n', 'd']
         end select
      end subroutine add_vertical_colorbar
   end subroutine rhyme_plotter_canvas_add_colorbar
end submodule canvas_add_colorbar_smod
