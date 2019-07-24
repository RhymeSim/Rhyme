submodule ( rhyme_plotter ) canvas_reset_smod
contains
  pure module subroutine rhyme_plotter_canvas_reset ( canvas )
    implicit none

    class ( plotter_canvas_t ), intent ( inout ) :: canvas

    canvas%table = ''
  end subroutine rhyme_plotter_canvas_reset
end submodule canvas_reset_smod
