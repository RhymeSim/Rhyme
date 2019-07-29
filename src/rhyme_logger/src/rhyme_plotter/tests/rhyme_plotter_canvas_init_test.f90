logical function rhyme_plotter_canvas_init_test () result ( failed )
  use rhyme_plotter

  implicit none

  type ( plotter_canvas_t ) :: canvas

  call canvas%init( 32, 16 )

  failed = &
  lbound( canvas%grid, dim=1 ) .ne. 1 - 16 &
  .or. ubound( canvas%grid, dim=1 ) .ne. 32 + 16 &
  .or. lbound( canvas%grid, dim=2 ) .ne. 1 - 4 &
  .or. ubound( canvas%grid, dim=2 ) .ne. 16 + 4
end function rhyme_plotter_canvas_init_test
