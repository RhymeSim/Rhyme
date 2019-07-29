submodule ( rhyme_plotter ) canvas_init_smod
contains
  pure module subroutine rhyme_plotter_canvas_init ( canvas, x, y )
    implicit none

    class ( plotter_canvas_t ), intent ( inout ) :: canvas
    integer, intent ( in ) :: x, y

    canvas%x = x
    canvas%y = y

    canvas%lbound_x = 1 - offset_x
    canvas%ubound_x = x + offset_x
    canvas%lbound_y = 1 - offset_y
    canvas%ubound_y = y + offset_y

    ! default values
    canvas%axes%is_on = .false.
    canvas%axes%scale = plid%linear

    if ( allocated( canvas%grid ) ) deallocate( canvas%grid )

    allocate( canvas%grid( &
      canvas%lbound_x:canvas%ubound_x, &
      canvas%lbound_y:canvas%ubound_y, &
      2 &
    ))

    call canvas%clear
  end subroutine rhyme_plotter_canvas_init
end submodule canvas_init_smod
