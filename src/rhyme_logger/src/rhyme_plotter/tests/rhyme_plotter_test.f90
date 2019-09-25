logical function rhyme_plotter_test () result ( failed )
  use rhyme_plotter

  implicit none

  integer, parameter :: d1len = 1e6
  integer, parameter :: d2len = 1e3
  integer, parameter :: d3len = 1e2
  integer :: dl = d1len

  type ( plotter_canvas_t ) :: canvas

  type ( plotter_histogram_t ) :: hist1_dr, hist1_d
  type ( plotter_histogram_t ) :: hist2_dr, hist2_d
  type ( plotter_histogram_t ) :: hist3_dr, hist3_d

  real ( kind=8 ), dimension ( d1len ) :: d, r, dr
  real ( kind=8 ), dimension ( d2len, d2len ) :: d2, r2, dr2
  real ( kind=8 ), dimension ( d3len, d3len, d3len ) :: d3, r3, dr3

  call random_seed( size = dl )

  call canvas%init( 80, 20 )


  ! 1D
  call random_number( d )
  call random_number( r )

  dr = d * r

  hist1_dr = rhyme_plotter_histogram( dr, 80, plid%linear, normalized=.false. )
  hist1_d = rhyme_plotter_histogram( d, 80, plid%linear, normalized=.false. )

  call canvas%add_axis( plid%right, 6, &
    [ minval(hist1_d%counts), maxval(hist1_d%counts) ], &
    scale=plid%linear, label='rho (kg / m^3)', color=colors%green )

  call canvas%add_axis( plid%top, 8, &
    [ minval(d), maxval(d) ], scale=plid%linear, label='T (K)', color=colors%green )

  call hist1_d%draw_on( canvas, xaxis=plid%top, yaxis=plid%right, color=colors%green )

  call canvas%add_axis( plid%left, 6, &
    [ minval(hist1_dr%counts), maxval(hist1_dr%counts) ], &
    scale=plid%linear, label='rho (kg / m^3)', color=colors%red )

  call canvas%add_axis( plid%bottom, 8, &
    [ minval(dr), maxval(dr) ], scale=plid%linear, label='P (Pa)', color=colors%red )

  call hist1_dr%draw_on( canvas, color=colors%red )

  call canvas%plot

  open( 1234, file='plot1d.txt', action='write' )
  call canvas%plot(1234, colored=.false. )
  close(1234)

  call canvas%clear

  ! 2D
  call random_number( d2 )
  call random_number( r2 )

  dr2 = d2 * r2

  hist2_dr = rhyme_plotter_histogram( dr2, 256, plid%log, normalized=.false. )
  hist2_d = rhyme_plotter_histogram( d2, 256, plid%log, normalized=.false. )

  call canvas%add_axis( plid%left, 5, &
    [ minval(hist2_dr%counts), maxval(hist2_dr%counts) ], &
    scale=plid%linear, label='rho (kg / m^3)', color=colors%yellow )

  call canvas%add_axis( plid%top, 7, &
    [ 1d4 * minval(dr), maxval(dr) ], scale=plid%log, label='P (Pa)', color=colors%yellow )

  call hist2_dr%draw_on( canvas, xaxis=plid%top, color=colors%yellow )


  call canvas%add_axis( plid%right, 5, &
    [ minval(hist2_d%counts), maxval(hist2_d%counts) ], &
    scale=plid%linear, label='rho (kg / m^3)', color=colors%indigo )

  call canvas%add_axis( plid%bottom, 7, &
    [ 1d4 * minval(d), maxval(d) ], scale=plid%log, label='T (K)', color=colors%indigo )

  call hist2_d%draw_on( canvas, yaxis=plid%right, color=colors%indigo )

  call canvas%plot

  open( 1234, file='plot2d.txt', action='write' )
  call canvas%plot(1234, colored=.false. )
  close(1234)

  call canvas%clear


  ! 3D
  call random_number( d3 )
  call random_number( r3 )

  dr3 = d3 * r3

  hist3_dr = rhyme_plotter_histogram( dr3, 256, plid%log, normalized=.false. )

  call canvas%add_axis( plid%right, 7, &
    [ minval(hist3_d%counts), maxval(hist3_d%counts) ], &
    scale=plid%linear, label='rho (kg / m^3)', color=colors%blue )

  call canvas%add_axis( plid%top, 4, &
    [ minval(d), maxval(d) ], scale=plid%log, label='T (K)', color=colors%blue )

  call canvas%add_axis( plid%left, 7, &
    [ minval(hist3_dr%counts), maxval(hist3_dr%counts) ], &
    scale=plid%linear, label='rho (kg / m^3)', color=colors%violet )

  call canvas%add_axis( plid%bottom, 9, &
    [ 1d5 * minval(dr), maxval(dr) ], scale=plid%log, label='P (Pa)', color=colors%violet )

  call hist3_dr%draw_on( canvas, color=colors%violet )

  call canvas%plot

  open( 1234, file='plot3d.txt', action='write' )
  call canvas%plot(1234, colored=.false. )
  close(1234)

  call canvas%clear
  

  ! To see the output set failed to .true.
  failed = .false.
end function rhyme_plotter_test
