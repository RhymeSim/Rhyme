submodule(rhyme_logger) plot_image_smod
contains
module subroutine rhyme_logger_plot_image( &
   this, values, xrange, yrange, labels, cs_range, cs_scale, colorscheme)
   implicit none

   class(logger_t), intent(inout) :: this
   real(kind=8), intent(in) :: values(:, :)
   real(kind=8), intent(in) :: xrange(2), yrange(2)
   character(len=32), intent(in), optional :: labels
   real(kind=8), intent(in), optional :: cs_range(2)
   integer, intent(in), optional :: cs_scale
   type(colorscheme_t), intent(in), optional :: colorscheme

   integer, parameter :: res = 72

   type(plotter_canvas_t) :: canvas
   type(plotter_image_t) :: image
   character(len=32) :: l(2)
   real(kind=8) :: csr(2)
   integer :: css
   type(colorscheme_t) :: cs

   if (present(labels)) then
      l = labels
   else
      l = ['X', 'Y']
   end if

   if (present(cs_range)) then
      csr = cs_range
   else
      csr = [minval(values), maxval(values)]
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

   call canvas%init(res, res/2)

   image%x%scale = plid%linear
   image%x%min = xrange(1)
   image%x%max = xrange(2)
   image%x%resolution = size(values, dim=1)
   image%y%scale = plid%linear
   image%y%min = yrange(1)
   image%y%max = yrange(2)
   image%y%resolution = size(values, dim=2)

   call canvas%add_axis( &
      plid%bottom, 5, xrange, scale=plid%linear, &
      label=trim(l(1)), color=tc%blue)
   call canvas%add_axis( &
      plid%left, 5, yrange, scale=plid%linear, &
      label=trim(l(2)), color=tc%blue)

   call canvas%draw( &
      image, values, xaxis_op=plid%bottom, yaxis_op=plid%left, &
      cs_min_op=csr(1), cs_max_op=csr(2), cs_scale_op=css)

   call canvas%add_colorbar(cs, csr(1), csr(2), css, plid%right, 7)

   call canvas%plot
end subroutine rhyme_logger_plot_image
end submodule plot_image_smod
