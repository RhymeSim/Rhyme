submodule(rhyme_plotter) canvas_clear_smod
contains
   pure module subroutine rhyme_plotter_canvas_clear(canvas)
      implicit none

      class(plotter_canvas_t), intent(inout) :: canvas

      canvas%grid = ''
   end subroutine rhyme_plotter_canvas_clear
end submodule canvas_clear_smod
