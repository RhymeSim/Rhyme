module rhyme_plotter
  implicit none

  integer, parameter, private :: offset_x = 16
  integer, parameter, private :: offset_y = 4
  integer, parameter, private :: max_nticks = 16
  integer, parameter, private :: max_nbins = 128

  integer, parameter :: ucs4  = selected_char_kind( 'ISO_10646' )

  type, private :: rhyme_plotter_indices
    integer :: unset = 0
    integer :: top = 1, bottom = 2, left = 3, right = 4
    integer :: log = 100, linear = 101
  end type rhyme_plotter_indices

  type ( rhyme_plotter_indices ), parameter :: plid = rhyme_plotter_indices()


  type, private :: plotter_canvas_axis_t
    logical :: is_on = .false.
    real ( kind=8 ) :: dx = 0d0, min = 0d0, max = 0d0
    real ( kind=8 ) :: tick_width_px = 0d0
    integer :: scale = plid%linear, n_ticks
    character ( len=12 ) :: color
  end type plotter_canvas_axis_t

  type plotter_canvas_t
    integer :: x = plid%unset, y = plid%unset
    type ( plotter_canvas_axis_t ) :: axes(4)
    character ( len=32, kind=ucs4 ), allocatable :: table(:, :)
  contains
    procedure :: init => rhyme_plotter_canvas_init
    procedure :: add_axis => rhyme_plotter_canvas_add_axis
    procedure :: plot => rhyme_plotter_canvas_plot
  end type plotter_canvas_t


  type plotter_histogram_t
    integer :: nbins, scale
    real ( kind=8 ) :: min, max, dx, base
    real ( kind=8 ), dimension( max_nbins ) :: counts, bin_centers
  contains
    procedure :: draw_on => rhyme_plotter_histogram_draw_on
  end type plotter_histogram_t


  type, private :: terminal_colors_t
    character ( len=12 ) :: rd = achar(27)//"[0;1;31;91m" ! Red
    character ( len=12 ) :: gn = achar(27)//"[0;1;32;92m" ! Green
    character ( len=12 ) :: yl = achar(27)//"[0;1;33;93m" ! Yellow
    character ( len=12 ) :: ig = achar(27)//"[0;1;34;94m" ! Indigo
    character ( len=12 ) :: vt = achar(27)//"[0;1;35;95m" ! Violet
    character ( len=12 ) :: bl = achar(27)//"[0;1;36;96m" ! Blue
    character ( len=4 ) :: nc = achar(27)//"[0m" ! Reset
  end type terminal_colors_t

  type ( terminal_colors_t ), parameter :: tc = terminal_colors_t ()


  interface
    pure module subroutine rhyme_plotter_canvas_init ( canvas, x, y )
      class ( plotter_canvas_t ), intent ( inout ) :: canvas
      integer, intent ( in ) :: x, y
    end subroutine rhyme_plotter_canvas_init

    module subroutine rhyme_plotter_canvas_add_axis ( canvas, axis, n_ticks, &
      minmax, scale, label, color )
      class ( plotter_canvas_t ), intent ( inout ) :: canvas
      integer, intent ( in ) :: axis, n_ticks
      real ( kind=8 ), intent ( in ) :: minmax(2)
      integer, intent ( in ), optional :: scale
      character ( len=* ), intent ( in ), optional :: label, color
    end subroutine rhyme_plotter_canvas_add_axis

    module subroutine rhyme_plotter_canvas_plot ( canvas )
      class ( plotter_canvas_t ), intent ( inout ) :: canvas
    end subroutine rhyme_plotter_canvas_plot

    pure module function rhyme_plotter_histogram_1d ( d, nbins, &
      scale, minmax, base, normalized ) result ( hist )
      real ( kind=8 ), intent ( in ) :: d(:)
      integer, intent ( in ) :: nbins
      integer, intent ( in ) :: scale
      real ( kind=8 ), intent ( in ), optional :: minmax(2)
      real ( kind=8 ), intent ( in ), optional :: base
      logical, intent ( in ), optional :: normalized
      type ( plotter_histogram_t ) :: hist
    end function rhyme_plotter_histogram_1d

    module subroutine rhyme_plotter_histogram_draw_on ( hist, canvas, &
      xaxis, yaxis, color )
      class ( plotter_histogram_t ), intent ( in ) :: hist
      type ( plotter_canvas_t ), intent ( inout ) :: canvas
      integer, intent ( in ), optional :: xaxis, yaxis
    character ( len=* ), intent ( in ), optional :: color
    end subroutine rhyme_plotter_histogram_draw_on
  end interface

  interface rhyme_plotter_histogram
    procedure rhyme_plotter_histogram_1d
  end interface rhyme_plotter_histogram
end module rhyme_plotter
