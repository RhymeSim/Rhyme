logical function rhyme_plotter_canvas_draw_image_test() result(failed)
   use rhyme_plotter

   implicit none

   integer, parameter :: res = 72
   integer :: seed = res*res

   type(plotter_canvas_t) :: canvas
   type(plotter_image_t) :: image

   real(kind=8) :: values(1024, 1024)

   call random_seed(seed)
   call random_number(values)

   call rhyme_color_init
   call canvas%init(res, res/2)

   image%x%scale = plid%linear
   image%x%min = -10
   image%x%max = 10
   image%x%resolution = 1024
   image%y%scale = plid%linear
   image%y%min = -10
   image%y%max = 10
   image%y%resolution = 1024

   call canvas%add_axis( &
      plid%bottom, 5, [-10d0, 10d0], scale=plid%linear, &
      label='X', color=tc%blue)
   call canvas%add_axis( &
      plid%left, 5, [-10d0, 10d0], scale=plid%linear, &
      label='Y', color=tc%blue)

   call canvas%draw( &
      image, values, xaxis_op=plid%bottom, yaxis_op=plid%left, &
      cs_min_op=minval(values), cs_max_op=maxval(values), &
      cs_scale_op=plid%linear)

   call canvas%add_colorbar( &
      colorschemes(csid%magma_grey), &
      minval(values), maxval(values), &
      plid%log, plid%right, 7)

   call canvas%plot

   ! To see the output set failed to .true.
   failed = .false.
end function rhyme_plotter_canvas_draw_image_test
