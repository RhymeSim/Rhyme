submodule(rhyme_logger) plot_2d_histogram_smod
contains
module subroutine rhyme_logger_plot_2d_histogram( &
   this, xvalues, yvalues, nbins, bin_scales, xdomain, ydomain, &
   normalized, labels, cs_range, cs_scale, colorscheme, axes_scales)
   implicit none

   class(logger_t), intent(inout) :: this
   real(kind=8), intent(in) :: xvalues(:), yvalues(:)
   integer, intent(in), optional :: nbins(2), bin_scales(2)
   real(kind=8), intent(in), optional :: xdomain(2), ydomain(2)
   logical, intent(in), optional :: normalized
   character(len=*), intent(in), optional :: labels(2)
   real(kind=8), intent(in), optional :: cs_range(2)
   integer, intent(in), optional :: cs_scale
   type(colorscheme_t), intent(in), optional :: colorscheme
   integer, intent(in), optional :: axes_scales(2)

   integer, parameter :: res(2) = [72, 30]

   type(plotter_canvas_t) :: canvas
   type(plotter_2d_histogram_t) :: histogram

   integer :: nb(2), bs(2), axsc(2)
   real(kind=8) :: xd(2), yd(2)
   logical :: norm
   character(len=128) :: l(2)
   real(kind=8) :: csr(2)
   integer :: css
   type(colorscheme_t) :: cs

   call canvas%init(res(1), res(2))

   if (present(nbins)) then
      nb = nbins
   else
      nb = [res(1), 2*res(2)]
   end if

   if (present(bin_scales)) then
      bs = bin_scales
   else
      bs = [plid%linear, plid%linear]
   end if

   if (present(axes_scales)) then
      axsc = axes_scales
   else
      axsc = [plid%linear, plid%linear]
   end if

   if (present(xdomain)) then
      xd = xdomain
   else
      xd = [minval(xvalues), maxval(xvalues)]
   end if

   if (present(ydomain)) then
      yd = ydomain
   else
      yd = [minval(yvalues), maxval(yvalues)]
   end if

   if (present(normalized)) then
      norm = normalized
   else
      norm = .false.
   end if

   if (present(labels)) then
      l = labels
   else
      l = ['X', 'Y']
   end if

   histogram = rhyme_plotter_two_d_histogram( &
               xvalues, yvalues, nb(1), nb(2), bs(1), bs(2), &
               xminmax=xd, yminmax=yd, normalized=norm)

   print *, minval(histogram%counts(1:nb(1), 1:nb(2)))
   print *, maxval(histogram%counts(1:nb(1), 1:nb(2)))

   if (present(cs_range)) then
      csr = cs_range
   else
      csr = [ &
            minval(histogram%counts(1:nb(1), 1:nb(2))), &
            maxval(histogram%counts(1:nb(1), 1:nb(2))) &
            ]
   end if

   if (present(cs_scale)) then
      css = cs_scale
   else
      css = plid%linear
   end if

   if (present(colorscheme)) then
      cs = colorscheme
   else
      cs = colorschemes(csid%magma_grey)
   end if

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
