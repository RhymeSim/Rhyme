logical function rhyme_logger_plot_histogram_test() result(failed)
   use rhyme_logger_factory

   implicit none

   type(logger_t) :: logger

   integer, parameter :: vlen = 1000000

   real(kind=8) :: values(vlen), pi
   integer :: i, seed = 1234

   pi = 4d0*datan(1d0)

   call random_seed(seed)
   call random_number(values)

   do i = 1, vlen
      ! values(i) = 1./(2*pi)*exp(-values(i)**2/2)
      values(i) = tan(2*pi*values(i))
   end do

   call logger%histogram( &
      values, nbins=80, bin_scale=plid%linear, normalized=.true., &
      labels=['X', 'Y'], axes_scales=[plid%linear, plid%log])

   ! To see the output set failed to .true.
   failed = .false.
end function rhyme_logger_plot_histogram_test
