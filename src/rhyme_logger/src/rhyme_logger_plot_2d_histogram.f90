submodule(rhyme_logger) plot_2d_histogram_smod
contains
module subroutine rhyme_logger_plot_2d_histogram( &
   logger, xvalues, yvalues, nbins, bin_scales, xdomain, ydomain, &
   normalized, labels, cs_range, cs_scale, colorscheme, axes_scales, &
   resolution)
   implicit none

   class(logger_t), intent(inout) :: logger
   real(kind=8), intent(in) :: xvalues(:), yvalues(:)
   integer, intent(in), optional :: nbins(2), bin_scales(2)
   real(kind=8), intent(in), optional :: xdomain(2), ydomain(2)
   logical, intent(in), optional :: normalized
   character(len=*), intent(in), optional :: labels(2)
   real(kind=8), intent(in), optional :: cs_range(2)
   integer, intent(in), optional :: cs_scale
   type(colorscheme_t), intent(in), optional :: colorscheme
   integer, intent(in), optional :: axes_scales(2)
   integer, intent(in), optional :: resolution(2)

   type(plotter_canvas_t) :: canvas
   type(plotter_2d_histogram_t) :: histogram

   integer :: nb(2), bs(2), axsc(2)
   real(kind=8) :: xd(2), yd(2)
   logical :: norm
   character(len=128) :: l(2)
   real(kind=8) :: csr(2)
   integer :: css
   type(colorscheme_t) :: cs
   integer :: res(2)

   if (.not. logger%unicode_plotting) return

   if (present(resolution)) then
      res = resolution
   else
      res = [72, 60]
   end if

   call logger%log('resolution', 'px', '=', res)

   call canvas%init(res(1), res(2)/2)

   if (present(nbins)) then
      nb = nbins
   else
      nb = [res(1), res(2)]
   end if

   call logger%log('nbins', '', '=', nb)

   if (present(bin_scales)) then
      bs = bin_scales
   else
      bs = [plid%linear, plid%linear]
   end if

   call logger%log('bin scales', '', '=', bs)

   if (present(axes_scales)) then
      axsc = axes_scales
   else
      axsc = [plid%linear, plid%linear]
   end if

   call logger%log('axes scales', '', '=', axsc)

   if (present(xdomain)) then
      xd = xdomain
   else
      if (present(axes_scales) .and. axes_scales(1) == plid%log) then
         xd(1) = minval(xvalues, xvalues > 0d0)
         xd(2) = maxval(xvalues, yvalues > 0d0)
      else
         xd = [minval(xvalues), maxval(xvalues)]
      end if
   end if

   call logger%log('x-axis range', '', '=', xd)

   if (present(ydomain)) then
      yd = ydomain
   else
      if (present(axes_scales) .and. axes_scales(2) == plid%log) then
         yd(1) = minval(yvalues, yvalues > 0d0)
         yd(2) = maxval(yvalues, yvalues > 0d0)
      else
         yd = [minval(yvalues), maxval(yvalues)]
      end if
   end if

   call logger%log('y-axis range', '', '=', yd)

   if (present(normalized)) then
      norm = normalized
   else
      norm = .false.
   end if

   call logger%log('normalized', '', '=', [norm])

   if (present(labels)) then
      l = labels
   else
      l = ['X', 'Y']
   end if

   call logger%log('calculating 2d histogram')
   histogram = rhyme_plotter_two_d_histogram( &
               xvalues, yvalues, nb(1), nb(2), bs(1), bs(2), &
               xminmax=xd, yminmax=yd, normalized=norm)

   if (present(cs_range)) then
      csr = cs_range
   else
      if (present(cs_scale) .and. cs_scale == plid%log) then
         csr(1) = minval(histogram%counts(1:nb(1), 1:nb(2)), histogram%counts(1:nb(1), 1:nb(2)) > 0d0)
         csr(2) = maxval(histogram%counts(1:nb(1), 1:nb(2)), histogram%counts(1:nb(1), 1:nb(2)) > 0d0)
      else
         csr(1) = minval(histogram%counts(1:nb(1), 1:nb(2)))
         csr(2) = maxval(histogram%counts(1:nb(1), 1:nb(2)))
      end if
   end if

   call logger%log('color scheme', '(range)', '=', csr)

   if (present(cs_scale)) then
      css = cs_scale
   else
      css = plid%linear
   end if

   call logger%log('color scheme', '(scale)', '=', [css])

   if (present(colorscheme)) then
      cs = colorscheme
   else
      cs = colorschemes(logger%colormap)
   end if

   call logger%log('color scheme', '', '=', [cs%name])

   call canvas%add_axis( &
      plid%bottom, 7, xd, scale=axsc(1), label=l(1), color=tc%blue)
   call canvas%add_axis( &
      plid%left, 5, yd, scale=axsc(2), label=l(2), color=tc%blue)

   call canvas%draw( &
      histogram, xaxis=plid%bottom, yaxis=plid%left, &
      cs_min_op=csr(1), cs_max_op=csr(2), cs_scale_op=css, &
      colorscheme_op=cs)

   call canvas%add_colorbar(cs, csr(1), csr(2), css, plid%right, 7)

   call canvas%plot
end subroutine rhyme_logger_plot_2d_histogram
end submodule plot_2d_histogram_smod
