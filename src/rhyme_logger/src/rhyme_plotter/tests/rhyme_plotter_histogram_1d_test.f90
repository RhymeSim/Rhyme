logical function rhyme_plotter_histogram_1d_test () result ( failed )
  use rhyme_plotter

  implicit none

  integer, parameter :: dlen = 1e6

  type ( plotter_canvas_t ) :: canvas
  type ( plotter_histogram_t ) :: hist

  real ( kind=8 ) :: d(dlen)
  integer :: dl = dlen

  call random_seed( size = dl )
  call random_number( d )

  hist = rhyme_plotter_histogram( d, 80, plid%log, normalized=.false. )

  call canvas%init( 80, 20 )

  call canvas%add_axis( plid%left, 6, [ minval(hist%counts), maxval(hist%counts) ], scale=plid%linear, label='rho (kg / m^3)' )
  call canvas%add_axis( plid%right, 10, [ minval(hist%counts), maxval(hist%counts) ], scale=plid%linear, label='rho (kg / m^3)' )
  call canvas%add_axis( plid%bottom, 6, [ minval(d), maxval(d) ], scale=plid%log, label='T (K)' )
  call canvas%add_axis( plid%top, 8, [ minval(d), maxval(d) * 10 ], scale=plid%log, label='P (Pa)' )

  call hist%draw_on( canvas, xaxis=plid%top )
  call hist%draw_on( canvas, yaxis=plid%right )

  call canvas%plot

  failed = .true.
end function rhyme_plotter_histogram_1d_test
