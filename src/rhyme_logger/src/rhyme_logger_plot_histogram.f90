submodule(rhyme_logger) plot_histogram_smod
contains
   module subroutine rhyme_logger_plot_histogram( &
      logger, values, nbins, bin_scale, domain, normalized, labels, &
      axes_scales, resolution, mask)
      implicit none

      class(logger_t), intent(inout) :: logger
      real(kind=8), intent(in) :: values(:)
      integer, intent(in), optional :: nbins, bin_scale
      real(kind=8), intent(in), optional :: domain(2)
      logical, intent(in), optional :: normalized
      character(len=*), intent(in), optional :: labels(2)
      integer, intent(in), optional :: axes_scales(2)
      integer, intent(in), optional :: resolution(2)
      integer, intent(in), optional :: mask

      type(plotter_canvas_t) :: canvas
      type(plotter_histogram_t) :: histogram

      integer :: nb, bs, axsc(2)
      logical :: norm
      character(len=128) :: l(2)
      real(kind=8) :: d(2)
      integer :: res(2), msk

      if (.not. logger%unicode_plotting) return

      if (present(mask)) then
         msk = mask
      else
         msk = plid%no_mask
      end if

      if (present(resolution)) then
         res = resolution
      else
         res = [128, 128]
      end if

      if (present(nbins)) then
         nb = nbins
      else
         nb = 40
      end if

      if (present(bin_scale)) then
         bs = bin_scale
      else
         bs = plid%linear
      end if

      if (present(normalized)) then
         norm = normalized
      else
         norm = .false.
      end if

      if (present(labels)) then
         l = labels
      else
         l = ['X        ', 'Frequency']
      end if

      if (present(axes_scales)) then
         axsc = axes_scales
      else
         axsc = [plid%linear, plid%linear]
      end if

      if (present(domain)) then
         d = domain
      else
         d = [ &
             rhyme_plotter_masked_min(values, msk), &
             rhyme_plotter_masked_max(values, msk) &
             ]
      end if

      call canvas%init(res(1), res(2))

      histogram = rhyme_plotter_histogram(values, nb, bs, normalized=norm, mask=msk)

      call logger%warn('# masked cells', '', '=', [histogram%n_masked])

      call canvas%add_axis( &
         plid%bottom, 7, d, scale=axsc(1), label=l(1), color=tc%blue)

      call canvas%add_axis( &
         plid%left, 6, &
         [minval(histogram%counts, histogram%counts > 0), maxval(histogram%counts)], &
         scale=axsc(2), label=l(2), color=tc%blue)

      call canvas%draw(histogram, xaxis=plid%bottom, yaxis=plid%left, color=tc%blue)

      call canvas%plot
      call canvas%clear

   end subroutine rhyme_logger_plot_histogram
end submodule plot_histogram_smod
