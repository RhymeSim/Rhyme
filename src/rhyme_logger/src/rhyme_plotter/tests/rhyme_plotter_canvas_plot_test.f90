logical function rhyme_plotter_canvas_plot_test () result ( failed )
  use rhyme_plotter

  implicit none

  type ( plotter_canvas_t ) :: canvas
  integer :: i, j

  call canvas%init( 80, 20 )

  do j = 1, 20
    do i = 1, 80
      canvas%table = char( int( z'0041' ), plid%ucs4 )
    end do
  end do

  call canvas%plot

  ! To see the output set failed to .true.
  failed = .false.
end function rhyme_plotter_canvas_plot_test
