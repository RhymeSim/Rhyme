submodule(rhyme_logger) plot_image_smod
contains
module subroutine rhyme_logger_plot_image( &
   logger, values, xrange, yrange, labels, cs_range, cs_scale, colorscheme, &
   axes_scales, auto_setup, resolution)
   implicit none

   class(logger_t), intent(inout) :: logger
   real(kind=8), intent(in) :: values(:, :)
   real(kind=8), intent(in) :: xrange(2), yrange(2)
   character(len=*), intent(in), optional :: labels(2)
   real(kind=8), intent(in), optional :: cs_range(2)
   integer, intent(in), optional :: cs_scale
   type(colorscheme_t), intent(in), optional :: colorscheme
   integer, intent(in), optional :: axes_scales(2)
   logical, intent(in), optional :: auto_setup
   integer, intent(in), optional :: resolution(2)

   type(plotter_canvas_t) :: canvas
   type(plotter_image_t) :: image
   character(len=128) :: l(2)
   real(kind=8) :: csr(2), cs_sign_range, minval_values, maxval_values
   integer :: css
   type(colorscheme_t) :: cs
   integer :: axsc(2)
   logical :: as, min_max_values_are_equal
   integer :: res(2)

   if (.not. logger%unicode_plotting) return

   minval_values = -huge(0d0)
   maxval_values = huge(0d0)

   if (present(auto_setup)) then
      as = auto_setup
      minval_values = minval(values)
      maxval_values = maxval(values)
      cs_sign_range = minval_values*maxval_values
      min_max_values_are_equal = &
         abs(maxval_values) > tiny(0d0) .and. &
         abs((minval_values - maxval_values)/maxval_values) < epsilon(0d0)
   else
      as = .false.
      cs_sign_range = 0d0
   end if

   if (present(labels)) then
      l = labels
   else
      l = ['X', 'Y']
   end if

   if (present(cs_range)) then
      csr = cs_range
   else
      if (as) then
         if (min_max_values_are_equal) then
            csr = [ &
                  minval_values - epsilon(0d0)*minval_values, &
                  maxval_values + epsilon(0d0)*maxval_values]
         else if (cs_sign_range < 0d0) then
            csr = [minval_values, maxval_values]
         else if (cs_sign_range > 0d0) then
            csr = [minval_values, maxval_values]
         else
            csr = [minval(values, values > 0d0), maxval_values]
         end if
      else
         csr = [minval_values, maxval_values]
      end if
   end if

   if (present(cs_scale)) then
      css = cs_scale
   else
      if (as) then
         if (min_max_values_are_equal) then
            css = plid%log
         else if (cs_sign_range < 0d0) then
            css = plid%linear
         else if (cs_sign_range > 0d0) then
            css = plid%log
         else
            css = plid%log
         end if
      else
         css = plid%linear
      end if
   end if

   if (present(colorscheme)) then
      cs = colorscheme
   else
      cs = colorschemes(logger%colormap)
   end if

   if (present(axes_scales)) then
      axsc = axes_scales
   else
      axsc = [plid%linear, plid%linear]
   end if

   if (present(resolution)) then
      res = resolution
   else
      res = [72, 72]
   end if

   call canvas%init(res(1), res(2)/2)

   image%x%scale = axsc(1)
   image%x%min = xrange(1)
   image%x%max = xrange(2)
   image%x%resolution = size(values, dim=1)
   image%y%scale = axsc(2)
   image%y%min = yrange(1)
   image%y%max = yrange(2)
   image%y%resolution = size(values, dim=2)

   call canvas%add_axis( &
      plid%bottom, 5, xrange, scale=axsc(1), &
      label=trim(l(1)), color=tc%blue)
   call canvas%add_axis( &
      plid%left, 5, yrange, scale=axsc(2), &
      label=trim(l(2)), color=tc%blue)

   call canvas%draw( &
      image, values, xaxis_op=plid%bottom, yaxis_op=plid%left, &
      cs_min_op=csr(1), cs_max_op=csr(2), cs_scale_op=css, &
      colorscheme_op=cs)

   call canvas%add_colorbar(cs, csr(1), csr(2), css, plid%right, 7)

   call canvas%plot
end subroutine rhyme_logger_plot_image
end submodule plot_image_smod
