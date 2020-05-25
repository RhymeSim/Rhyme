logical function rhyme_logger_plot_test() result(failed)
   use rhyme_logger_factory

   implicit none

   type(logger_t) :: logger

   integer :: i, j, seed = 1234
   real(kind=8) :: x, y
   real(kind=8) :: values(-512:512, -512:512)

   logger = logger_factory_generate('unicode-plotting')

   call random_seed(seed)
   call random_number(values)

   call rhyme_color_init()

   do j = -512, 512
      y = j*3d0/512
      do i = -512, 512
         x = i*3d0/512
         values(i, j) = sin(x**2 + y**2)
      end do
   end do

   call logger%plot(values, [-3d0, 3d0], [-3d0, 3d0])

   ! To see the output set failed to .true.
   failed = .false.
end function rhyme_logger_plot_test
