logical function rhyme_color_init_test() result(failed)
   use rhyme_color_factory

   implicit none

   integer :: n

   failed = .false.

   call rhyme_color_init()

   do n = 0, 255
      if (colors(n)%fg(3:4) /= '38' .or. colors(n)%bg(3:4) /= '48') then
         failed = .true.
      end if
   end do
end function rhyme_color_init_test
