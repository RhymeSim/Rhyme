submodule(rhyme_logger) plot_2d_histogram_3d_smod
contains

   module subroutine rhyme_logger_plot_2d_histogram_3d( &
      logger, xvalues, yvalues, nbins, bin_scales, xdomain, ydomain, &
      normalized, labels, cs_range, cs_scale, colorscheme, axes_scales, &
      resolution)
      implicit none

      class(logger_t), intent(inout) :: logger
      real(kind=8), intent(in) :: xvalues(:, :, :), yvalues(:, :, :)
      integer, intent(in), optional :: nbins(2), bin_scales(2)
      real(kind=8), intent(in), optional :: xdomain(2), ydomain(2)
      logical, intent(in), optional :: normalized
      character(len=*), intent(in), optional :: labels(2)
      real(kind=8), intent(in), optional :: cs_range(2)
      integer, intent(in), optional :: cs_scale
      type(colorscheme_t), intent(in), optional :: colorscheme
      integer, intent(in), optional :: axes_scales(2)
      integer, intent(in), optional :: resolution(2)

      real(kind=8), allocatable :: v(:, :)

      allocate (v(product(ubound(xvalues)), 2))

      v(:, 1) = pack( &
                xvalues( &
                1:ubound(xvalues, dim=1), &
                1:ubound(xvalues, dim=2), &
                1:ubound(xvalues, dim=3) &
                ), .true.)
      v(:, 2) = pack( &
                yvalues( &
                1:ubound(yvalues, dim=1), &
                1:ubound(yvalues, dim=2), &
                1:ubound(yvalues, dim=3) &
                ), .true.)

      call logger%plot_2d_histogram( &
         v(:, 1), v(:, 2), nbins=nbins, bin_scales=bin_scales, &
         xdomain=xdomain, ydomain=ydomain, normalized=normalized, &
         labels=labels, cs_range=cs_range, cs_scale=cs_scale, &
         colorscheme=colorscheme, axes_scales=axes_scales, &
         resolution=resolution)

      deallocate (v)
   end subroutine rhyme_logger_plot_2d_histogram_3d
end submodule plot_2d_histogram_3d_smod
