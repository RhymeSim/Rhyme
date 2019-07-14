logical function rhyme_plotter_canvas_add_axis_test () result ( failed )
  use rhyme_plotter

  implicit none

  type ( plotter_canvas_t ) :: canvas

  call canvas%init( 80, 20 )
  call canvas%add_axis( plid%left, 6, [ 0d0, 1d0 ], scale=plid%linear, label='rho (kg / m^3)' )
  call canvas%add_axis( plid%bottom, 8, [ 1d0, 1d7 ], scale=plid%log, label='T (K)' )
  call canvas%add_axis( plid%right, 6, [ 1d1, 6d1 ], scale=plid%linear, label='rho (kg / m^3)' )
  call canvas%add_axis( plid%top, 8, [ 1d0 / 2.23d3, 1d7 / 2.23d3 ], scale=plid%log, label='P (Pa)' )

  call canvas%plot

  ! To see the output set failed to .true.
  failed = .false.
end function rhyme_plotter_canvas_add_axis_test
