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

    if ( allocated( canvas%bw ) ) deallocate( canvas%bw )
    if ( allocated( canvas%clr ) ) deallocate( canvas%clr )

    allocate( canvas%bw( &
      1-offset_x:x+offset_x, &
      1-offset_y:y+offset_y &
    ))

    allocate( canvas%clr( &
      1-offset_x:x+offset_x, &
      1-offset_y:y+offset_y &
    ))

    call canvas%clear
  end subroutine rhyme_plotter_canvas_init
end submodule canvas_init_smod
