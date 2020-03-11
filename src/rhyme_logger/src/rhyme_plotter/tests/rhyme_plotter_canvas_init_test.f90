logical function rhyme_plotter_canvas_init_test() result(failed)
   use rhyme_plotter

   implicit none

   type(plotter_canvas_t) :: canvas

   call canvas%init(32, 16)

   failed = .false.
end function rhyme_plotter_canvas_init_test
