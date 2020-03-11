submodule(rhyme_color) rhyme_mh_init_smod
contains
module subroutine rhyme_color_init()
   implicit none

   integer :: r, g, b, n

   character(len=16), parameter :: str_fmt = '(A2,A5,I0.3,A1)'

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
end subroutine rhyme_color_init
end submodule rhyme_mh_init_smod
