logical function rhyme_plotter_histogram_1d_test () result ( failed )
  use rhyme_plotter

  implicit none

  integer, parameter :: dlen = 1e6

  type ( plotter_canvas_t ) :: canvas
  type ( plotter_histogram_t ) :: hist_dr, hist_d

  real ( kind=8 ), dimension ( dlen ) :: d, r, dr
  integer :: dl = dlen

  call random_seed( size = dl )
  call random_number( d )
  call random_number( r )

  call canvas%init( 80, 20 )

  dr = d * r

  hist_dr = rhyme_plotter_histogram( dr, 80, plid%log, normalized=.false. )
  hist_d = rhyme_plotter_histogram( d, 80, plid%log, normalized=.false. )

  call canvas%add_axis( plid%left, 6, &
    [ minval(hist_dr%counts), maxval(hist_dr%counts) ], &
    scale=plid%linear, label='rho (kg / m^3)', color=tc%rd )

  call canvas%add_axis( plid%bottom, 8, &
    [ minval(dr), maxval(dr) ], scale=plid%linear, label='P (Pa)', color=tc%rd )

  call hist_dr%draw_on( canvas, color=tc%rd )


  call canvas%add_axis( plid%right, 6, &
    [ minval(hist_d%counts), maxval(hist_d%counts) ], &
    scale=plid%linear, label='rho (kg / m^3)', color=tc%ig )

  call canvas%add_axis( plid%top, 8, &
    [ minval(d), maxval(d) ], scale=plid%log, label='T (K)', color=tc%ig )

  call hist_d%draw_on( canvas, xaxis=plid%top, yaxis=plid%right, color=tc%ig )

  call canvas%plot

  failed = .false.
end function rhyme_plotter_histogram_1d_test
