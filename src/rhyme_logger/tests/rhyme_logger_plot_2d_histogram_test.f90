logical function rhyme_logger_plot_2d_histogram_test() result(failed)
   use rhyme_logger_factory

   implicit none

   type(logger_t) :: logger

   integer, parameter :: vlen = 23456

   integer :: seed = 1234
   real(kind=8) :: xvalues(vlen), yvalues(vlen), pi

   call random_seed(seed)
   call random_number(xvalues)
   call random_number(yvalues)

   pi = 4d0*datan(1d0)

   xvalues = sinh(xvalues*2*pi - pi)
   yvalues = cos(yvalues*2*pi - pi)

   call rhyme_color_init

   call logger%histogram( &
      xvalues, yvalues, xdomain=[-pi, pi], ydomain=[-pi/3, pi/3], &
      cs_range=[1d-5, 1d-3], &
      cs_scale=plid%log, normalized=.true.)

   ! To see the output set failed to .true.
   failed = .false.
end function rhyme_logger_plot_2d_histogram_test
