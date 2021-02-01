module rhyme_plotter
   use rhyme_color

   implicit none

   integer, parameter, private :: offset_x = 15
   integer, parameter, private :: offset_y = 4
   integer, parameter, private :: canvas_border = 2
   integer, parameter, private :: max_nticks = 16
   integer, parameter, private :: max_nbins = 256
   integer, parameter, private :: max_image_resolution = 128

   integer, parameter :: ucs4 = selected_char_kind('ISO_10646')

   type, private :: rhyme_plotter_indices
      integer :: unset = 0
      integer :: top = 1, bottom = 2, left = 3, right = 4
      integer :: log = 100, linear = 101
      integer :: black_and_white = 1, bw = 1
      integer :: colored = 2, clr = 2
      integer :: no_mask = 1000, gtr_zero = 1001, less_zero = 1002, &
                 gtr_eq_zero = 1003, less_eq_zero = 1004
   end type rhyme_plotter_indices

   type(rhyme_plotter_indices), parameter :: plid = rhyme_plotter_indices()

   type, private :: plotter_canvas_axis_t
      logical :: is_on = .false.
      real(kind=8) :: dx = 0d0, min = 0d0, max = 0d0
      real(kind=8) :: tick_width_px = 0d0
      real(kind=8) :: ticks(max_nticks)
      character(len=16) :: tick_labels(max_nticks)
      integer :: scale = plid%linear, n_ticks
      character(len=12) :: color
   end type plotter_canvas_axis_t

   type plotter_canvas_t
      integer :: x = plid%unset, y = plid%unset
      integer :: lbound_x = plid%unset, ubound_x = plid%unset
      integer :: lbound_y = plid%unset, ubound_y = plid%unset
      type(plotter_canvas_axis_t) :: axes(4)
      character(len=64, kind=ucs4), allocatable :: grid(:, :, :)
   contains
      procedure :: init => rhyme_plotter_canvas_init
      procedure :: add_axis => rhyme_plotter_canvas_add_axis
      procedure :: add_corner => rhyme_plotter_canvas_add_corner
      procedure :: add_colorbar => rhyme_plotter_canvas_add_colorbar
      procedure :: draw_histogram => rhyme_plotter_canvas_draw_histogram
      procedure :: draw_2d_histogram => rhyme_plotter_canvs_draw_2d_histogram
      procedure :: draw_image => rhyme_plotter_canvas_draw_image
      procedure :: plot => rhyme_plotter_canvas_plot
      procedure :: clear => rhyme_plotter_canvas_clear
      generic :: draw => draw_histogram, draw_2d_histogram, draw_image
   end type plotter_canvas_t

   type plotter_histogram_t
      integer :: nbins, scale, n_masked = 0
      real(kind=8) :: min, max, dx, base
      real(kind=8), dimension(max_nbins) :: counts, bin_centers
   end type plotter_histogram_t

   type plotter_histogram_axis_t
      integer :: nbins = 0, scale = plid%linear, n_masked = 0
      real(kind=8) :: min = 0, max = 0
   end type plotter_histogram_axis_t

   type plotter_2d_histogram_t
      type(plotter_histogram_axis_t) :: x, y
      real(kind=8), dimension(max_nbins, max_nbins) :: counts = 0
   end type plotter_2d_histogram_t

   type plotter_image_axis_t
      integer :: resolution = 0, scale = plid%linear
      real(kind=8) :: min = 0, max = 0
   end type plotter_image_axis_t

   type plotter_image_t
      type(plotter_image_axis_t) :: x, y
   end type plotter_image_t

   interface
      module subroutine rhyme_plotter_canvas_init(canvas, x, y)
         class(plotter_canvas_t), intent(inout) :: canvas
         integer, intent(in) :: x, y
      end subroutine rhyme_plotter_canvas_init

      module subroutine rhyme_plotter_canvas_add_axis( &
         canvas, axis, n_ticks, minmax, scale, label, color)
         class(plotter_canvas_t), intent(inout) :: canvas
         integer, intent(in) :: axis, n_ticks
         real(kind=8), intent(in) :: minmax(2)
         integer, intent(in), optional :: scale
         character(len=*), intent(in), optional :: label, color
      end subroutine rhyme_plotter_canvas_add_axis

      module subroutine rhyme_plotter_canvas_add_corner( &
         canvas, xaxis, yaxis)
         integer, intent(in) :: xaxis, yaxis
         class(plotter_canvas_t), intent(inout) :: canvas
      end subroutine rhyme_plotter_canvas_add_corner

      module subroutine rhyme_plotter_canvas_add_colorbar( &
         canvas, colorscheme, cs_min, cs_max, cs_scale, location_op, nticks_op)
         class(plotter_canvas_t), intent(inout) :: canvas
         type(colorscheme_t), intent(in) :: colorscheme
         real(kind=8), intent(in) :: cs_min, cs_max
         integer, intent(in) :: cs_scale
         integer, intent(in), optional :: location_op, nticks_op
      end subroutine rhyme_plotter_canvas_add_colorbar

      module subroutine rhyme_plotter_canvas_draw_histogram( &
         canvas, hist, xaxis, yaxis, color)
         implicit none

         class(plotter_canvas_t), intent(inout) :: canvas
         type(plotter_histogram_t), intent(in) :: hist
         integer, intent(in), optional :: xaxis, yaxis
         character(len=*), intent(in), optional :: color
      end subroutine rhyme_plotter_canvas_draw_histogram

      module subroutine rhyme_plotter_canvs_draw_2d_histogram( &
         canvas, hist, xaxis, yaxis, colorscheme_op, &
         cs_min_op, cs_max_op, cs_scale_op)
         class(plotter_canvas_t), intent(inout) :: canvas
         type(plotter_2d_histogram_t), intent(in) :: hist
         integer, intent(in), optional :: xaxis, yaxis
         type(colorscheme_t), intent(in), optional :: colorscheme_op
         real(kind=8), intent(in), optional :: cs_min_op, cs_max_op
         integer, intent(in), optional :: cs_scale_op
      end subroutine rhyme_plotter_canvs_draw_2d_histogram

      module subroutine rhyme_plotter_canvas_draw_image( &
         canvas, image, values, xaxis_op, yaxis_op, colorscheme_op, &
         cs_min_op, cs_max_op, cs_scale_op)
         class(plotter_canvas_t), intent(inout) :: canvas
         type(plotter_image_t), intent(in) :: image
         real(kind=8), intent(in) :: values(:, :)
         integer, intent(in), optional :: xaxis_op, yaxis_op
         type(colorscheme_t), intent(in), optional :: colorscheme_op
         real(kind=8), intent(in), optional :: cs_min_op, cs_max_op
         integer, intent(in), optional :: cs_scale_op
      end subroutine rhyme_plotter_canvas_draw_image

      module subroutine rhyme_plotter_canvas_plot(canvas, output, colored)
         class(plotter_canvas_t), intent(inout) :: canvas
         integer, intent(in), optional :: output
         logical, intent(in), optional :: colored
      end subroutine rhyme_plotter_canvas_plot

      pure module subroutine rhyme_plotter_canvas_clear(canvas)
         class(plotter_canvas_t), intent(inout) :: canvas
      end subroutine rhyme_plotter_canvas_clear

      pure module function rhyme_plotter_histogram_1d( &
         d, nbins, scale, minmax, base, normalized, mask) result(hist)
         real(kind=8), intent(in) :: d(:)
         integer, intent(in) :: nbins
         integer, intent(in) :: scale
         real(kind=8), intent(in), optional :: minmax(2)
         real(kind=8), intent(in), optional :: base
         logical, intent(in), optional :: normalized
         integer, intent(in), optional :: mask
         type(plotter_histogram_t) :: hist
      end function rhyme_plotter_histogram_1d

      pure module function rhyme_plotter_histogram_2d( &
         d, nbins, scale, minmax, base, normalized, mask) result(hist)
         real(kind=8), intent(in) :: d(:, :)
         integer, intent(in) :: nbins
         integer, intent(in) :: scale
         real(kind=8), intent(in), optional :: minmax(2)
         real(kind=8), intent(in), optional :: base
         logical, intent(in), optional :: normalized
         integer, intent(in), optional :: mask
         type(plotter_histogram_t) :: hist
      end function rhyme_plotter_histogram_2d

      pure module function rhyme_plotter_histogram_3d( &
         d, nbins, scale, minmax, base, normalized, mask) result(hist)
         real(kind=8), intent(in) :: d(:, :, :)
         integer, intent(in) :: nbins
         integer, intent(in) :: scale
         real(kind=8), intent(in), optional :: minmax(2)
         real(kind=8), intent(in), optional :: base
         logical, intent(in), optional :: normalized
         integer, intent(in), optional :: mask
         type(plotter_histogram_t) :: hist
      end function rhyme_plotter_histogram_3d

      pure module function rhyme_plotter_two_d_histogram( &
         x, y, xbins, ybins, xscale, yscale, xminmax, yminmax, &
         normalized, mask) result(hist2d)
         real(kind=8), intent(in) :: x(:), y(:)
         integer, intent(in) :: xbins, ybins, xscale, yscale
         real(kind=8), intent(in), optional :: xminmax(2), yminmax(2)
         logical, intent(in), optional :: normalized
         integer, intent(in), optional :: mask(2)
         type(plotter_2d_histogram_t) :: hist2d
      end function rhyme_plotter_two_d_histogram

      pure module function rhyme_plotter_is_masked_scalar_r8(x, mask) result(masked)
         real(kind=8), intent(in) :: x
         integer, intent(in) :: mask
         logical :: masked
      end function rhyme_plotter_is_masked_scalar_r8

      pure module function rhyme_plotter_masked_max_1d_array_r8(arr, mask) result(maximum)
         real(kind=8), intent(in) :: arr(:)
         integer, intent(in) :: mask
         real(kind=8) :: maximum
      end function rhyme_plotter_masked_max_1d_array_r8

      pure module function rhyme_plotter_masked_max_2d_array_r8(arr, mask) result(maximum)
         real(kind=8), intent(in) :: arr(:, :)
         integer, intent(in) :: mask
         real(kind=8) :: maximum
      end function rhyme_plotter_masked_max_2d_array_r8

      pure module function rhyme_plotter_masked_max_3d_array_r8(arr, mask) result(maximum)
         real(kind=8), intent(in) :: arr(:, :, :)
         integer, intent(in) :: mask
         real(kind=8) :: maximum
      end function rhyme_plotter_masked_max_3d_array_r8

      pure module function rhyme_plotter_masked_min_1d_array_r8(arr, mask) result(minimum)
         real(kind=8), intent(in) :: arr(:)
         integer, intent(in) :: mask
         real(kind=8) :: minimum
      end function rhyme_plotter_masked_min_1d_array_r8

      pure module function rhyme_plotter_masked_min_2d_array_r8(arr, mask) result(minimum)
         real(kind=8), intent(in) :: arr(:, :)
         integer, intent(in) :: mask
         real(kind=8) :: minimum
      end function rhyme_plotter_masked_min_2d_array_r8

      pure module function rhyme_plotter_masked_min_3d_array_r8(arr, mask) result(minimum)
         real(kind=8), intent(in) :: arr(:, :, :)
         integer, intent(in) :: mask
         real(kind=8) :: minimum
      end function rhyme_plotter_masked_min_3d_array_r8
   end interface

   interface rhyme_plotter_histogram
      procedure rhyme_plotter_histogram_1d
      procedure rhyme_plotter_histogram_2d
      procedure rhyme_plotter_histogram_3d
   end interface rhyme_plotter_histogram

   interface rhyme_plotter_masked_max
      procedure rhyme_plotter_masked_max_1d_array_r8
      procedure rhyme_plotter_masked_max_2d_array_r8
      procedure rhyme_plotter_masked_max_3d_array_r8
   end interface rhyme_plotter_masked_max

   interface rhyme_plotter_masked_min
      procedure rhyme_plotter_masked_min_1d_array_r8
      procedure rhyme_plotter_masked_min_2d_array_r8
      procedure rhyme_plotter_masked_min_3d_array_r8
   end interface rhyme_plotter_masked_min

   interface rhyme_plotter_is_masked
      procedure rhyme_plotter_is_masked_scalar_r8
   end interface rhyme_plotter_is_masked
end module rhyme_plotter
