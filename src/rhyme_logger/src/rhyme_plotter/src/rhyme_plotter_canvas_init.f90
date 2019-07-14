submodule ( rhyme_plotter ) canvas_init_smod
contains
  pure module subroutine rhyme_plotter_canvas_init ( this, x, y )
    implicit none

    class ( plotter_canvas_t ), intent ( inout ) :: this
    integer, intent ( in ) :: x, y

    this%x = x
    this%y = y

    this%added_axis = .false.
    this%axis_scale = plid%linear

    if ( allocated( this%table ) ) deallocate( this%table )

    allocate( this%table( &
      1-offset_x:x+offset_x, &
      1-offset_y:y+offset_y &
    ))

    this%table = ''
  end subroutine rhyme_plotter_canvas_init
end submodule canvas_init_smod
