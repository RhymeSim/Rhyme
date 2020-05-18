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

   colorschemes(csid%rainbow)%n = 21
   colorschemes(csid%rainbow)%pallet(csid%unknown) = colors(16)
   colorschemes(csid%rainbow)%pallet(csid%low_end) = colors(88)
   colorschemes(csid%rainbow)%pallet(csid%high_end) = colors(219)
   colorschemes(csid%rainbow)%pallet(1:21) = &
      [colors(160), colors(196), colors(208), colors(214), colors(220), colors(226), &
       colors(190), colors(154), colors(118), colors(46), colors(49), colors(50), &
       colors(51), colors(45), colors(39), colors(33), colors(105), colors(93), &
       colors(129), colors(165), colors(201)]

   colorschemes(csid%smooth_rainbow)%n = 10
   colorschemes(csid%smooth_rainbow)%pallet(csid%unknown) = colors(118)
   colorschemes(csid%smooth_rainbow)%pallet(csid%low_end) = colors(231)
   colorschemes(csid%smooth_rainbow)%pallet(csid%high_end) = colors(16)
   colorschemes(csid%smooth_rainbow)%pallet(1:10) = &
      [colors(239), colors(182), colors(133), colors(104), colors(109), &
       colors(114), colors(185), colors(215), colors(167), colors(88)]

   colorschemes(csid%viridis)%n = 12
   colorschemes(csid%viridis)%pallet(csid%unknown) = colors(197)
   colorschemes(csid%viridis)%pallet(csid%low_end) = colors(231)
   colorschemes(csid%viridis)%pallet(csid%high_end) = colors(16)
   colorschemes(csid%viridis)%pallet(1:12) = &
      [colors(54), colors(60), colors(61), colors(67), colors(73), colors(72), &
       colors(78), colors(114), colors(113), colors(149), colors(184), colors(226)]
end subroutine rhyme_color_init
end submodule rhyme_mh_init_smod
