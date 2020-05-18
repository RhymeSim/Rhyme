logical function rhyme_plotter_canvas_draw_2d_histogram_test() result(failed)
   use rhyme_plotter

   implicit none

   integer, parameter :: res = 72
   integer, parameter :: d1len = 1e6
   integer :: dl = d1len

   type(plotter_canvas_t) :: canvas
   type(plotter_2d_histogram_t) :: hist2d

   real(kind=8), dimension(d1len) :: dd, d, rr, r

   call random_seed(size=dl)

   call canvas%init(res, res/2)

   call random_number(dd)
   call random_number(rr)

   r = (rr*3d0 + epsilon(0d0))
   d = (dd*3d0 + epsilon(0d0))

   hist2d = rhyme_plotter_two_d_histogram( &
            r, d, res, res, plid%log, plid%log, &
            [minval(r), maxval(r)], [minval(d), maxval(d)])

   call canvas%add_axis( &
      plid%bottom, 7, [epsilon(0d0), 3d0], &
      scale=plid%log, label='X', color=tc%blue)

   call canvas%add_axis( &
      plid%left, 7, [epsilon(0d0), 3d0], &
      scale=plid%log, label='Y', color=tc%blue)

   call canvas%draw( &
      hist2d, xaxis=plid%bottom, yaxis=plid%left, &
      cs_min_op=minval(hist2d%counts(1:res, 1:res)), &
      cs_max_op=maxval(hist2d%counts(1:res, 1:res)), &
      cs_scale_op=plid%linear)

   call canvas%plot
   call canvas%clear

   r = (rr*6d0 - 3d0)
   d = (dd*6d0 - 3d0)

   hist2d = rhyme_plotter_two_d_histogram( &
            r, d, res, res, plid%linear, plid%linear, &
            [minval(r), maxval(r)], [minval(d), maxval(d)])

   call canvas%add_axis( &
      plid%bottom, 7, [-3d0, 3d0], &
      scale=plid%linear, label='X', color=tc%blue)

   call canvas%add_axis( &
      plid%left, 7, [-3d0, 3d0], &
      scale=plid%linear, label='Y', color=tc%blue)

   call canvas%draw( &
      hist2d, xaxis=plid%bottom, yaxis=plid%left, &
      cs_min_op=minval(hist2d%counts(1:res, 1:res)), &
      cs_max_op=maxval(hist2d%counts(1:res, 1:res)), &
      cs_scale_op=plid%linear)

   call canvas%add_colorbar( &
      colorschemes(csid%magma_grey), &
      minval(hist2d%counts(1:res, 1:res)), &
      maxval(hist2d%counts(1:res, 1:res)), &
      plid%linear, plid%right, 5)

   call canvas%plot
   call canvas%clear

   r = sinh(rr*6.28 - 3.14)
   d = cos(dd*6.28 - 3.14)

   hist2d = rhyme_plotter_two_d_histogram( &
            r, d, res, res, plid%linear, plid%linear, &
            [minval(r), maxval(r)], [minval(d), maxval(d)])

   call canvas%add_axis( &
      plid%bottom, 7, [minval(r), maxval(r)], &
      scale=plid%linear, label='X', color=tc%blue)

   call canvas%add_axis( &
      plid%left, 7, [minval(d), maxval(d)], &
      scale=plid%linear, label='Y', color=tc%blue)

   call canvas%draw( &
      hist2d, xaxis=plid%bottom, yaxis=plid%left, &
      cs_min_op=minval(hist2d%counts(1:res, 1:res)), &
      cs_max_op=maxval(hist2d%counts(1:res, 1:res)), &
      cs_scale_op=plid%log)

   call canvas%add_colorbar( &
      colorschemes(csid%magma_grey), &
      minval(hist2d%counts(1:res, 1:res)), &
      maxval(hist2d%counts(1:res, 1:res)), &
      plid%log, plid%right, 5)

   call canvas%plot
   call canvas%clear

   ! To see the output set failed to .true.
   failed = .false.
end function rhyme_plotter_canvas_draw_2d_histogram_test
