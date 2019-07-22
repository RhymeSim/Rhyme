submodule ( rhyme_plotter ) canvas_init_smod
contains
  pure module subroutine rhyme_plotter_canvas_init ( canvas, x, y )
    implicit none

    class ( plotter_canvas_t ), intent ( inout ) :: canvas
    integer, intent ( in ) :: x, y

    canvas%x = x
    canvas%y = y

    canvas%axes%is_on = .false.
    canvas%axes%scale = plid%linear

    if ( allocated( canvas%table ) ) deallocate( canvas%table )

    allocate( canvas%table( &
      1-offset_x:x+offset_x, &
      1-offset_y:y+offset_y &
    ))

    canvas%table = ''
  end subroutine rhyme_plotter_canvas_init
end submodule canvas_init_smod
