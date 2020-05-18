logical function rhyme_plotter_canvas_draw_histogram_test() result(failed)
   use rhyme_plotter

   implicit none

   type(plotter_canvas_t) :: canvas

   integer, parameter :: d1len = 1e6
   integer :: dl = d1len

   type(plotter_histogram_t) :: hist1_d
   real(kind=8), dimension(d1len) :: d

   call random_seed(size=dl)

   call rhyme_color_init
   call canvas%init(80, 20)

   call random_number(d)

   hist1_d = rhyme_plotter_histogram(d, 40, plid%linear, normalized=.false.)

   call canvas%add_axis( &
      plid%right, 6, &
      [minval(hist1_d%counts, hist1_d%counts > 0), maxval(hist1_d%counts)], &
      scale=plid%log, label='rho (kg / m^3)', color=tc%blue)

   call canvas%add_axis( &
      plid%top, 8, [minval(d), maxval(d)], &
      scale=plid%linear, label='T (K)', color=tc%blue)

   call canvas%draw(hist1_d, xaxis=plid%top, yaxis=plid%right, color=tc%blue)

   call canvas%plot
   call canvas%clear

! To see the output set failed to .true.
   failed = .false.
end function rhyme_plotter_canvas_draw_histogram_test
