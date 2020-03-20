submodule(rhyme_color) rhyme_mh_init_smod
contains
module subroutine rhyme_color_init()
   implicit none

   integer :: r, g, b, n

   character(len=16), parameter :: str_fmt = '(A2,A5,I0.3,A1)'

   ! Colors
   do n = 0, 15
      colors(n)%n = n
      colors(n)%r = -1
      colors(n)%g = -1
      colors(n)%b = -1

      write (colors(n)%fg, str_fmt) clrid%start, '38;5;', n, 'm'
      write (colors(n)%bg, str_fmt) clrid%start, '48;5;', n, 'm'
   end do

   do r = 0, 5
   do g = 0, 5
   do b = 0, 5
      n = 16 + 36*r + 6*g + b

      colors(n)%n = n
      colors(n)%r = r
      colors(n)%g = g
      colors(n)%b = b

      write (colors(n)%fg, str_fmt) clrid%start, '38;5;', n, 'm'
      write (colors(n)%bg, str_fmt) clrid%start, '48;5;', n, 'm'
   end do
   end do
   end do

   do n = 232, 255
      colors(n)%n = n
      colors(n)%r = -1
      colors(n)%g = -1
      colors(n)%b = -1

      write (colors(n)%fg, str_fmt) clrid%start, '38;5;', n, 'm'
      write (colors(n)%bg, str_fmt) clrid%start, '48;5;', n, 'm'
   end do

   ! Color Schemes
   colorschemes(csid%magma_grey)%n = 24
   colorschemes(csid%magma_grey)%pallet(csid%unknown) = colors(118)
   colorschemes(csid%magma_grey)%pallet(csid%low_end) = colors(15)
   colorschemes(csid%magma_grey)%pallet(csid%high_end) = colors(16)
   colorschemes(csid%magma_grey)%pallet(1:24) = &
      [colors(255), colors(253), colors(251), colors(249), colors(247), colors(245), &
       colors(243), colors(241), colors(239), colors(237), colors(235), colors(233), &
       colors(230), colors(229), colors(222), colors(216), colors(210), colors(204), &
       colors(168), colors(127), colors(91), colors(54), colors(17), colors(16)]

end subroutine rhyme_color_init
end submodule rhyme_mh_init_smod
