module rhyme_plotter
  implicit none

  integer, parameter, private :: offset_x = 16
  integer, parameter, private :: offset_y = 4
  integer, parameter, private :: max_nticks = 16
  integer, parameter, private :: min_xtick_len = 11

  type, private :: rhyme_plotter_indices
    integer :: unset = 0
    integer :: top = 1, bottom = 2, left = 3, right = 4
    integer :: ucs4  = selected_char_kind( 'ISO_10646' )
    integer :: log = 100, linear = 101
  end type rhyme_plotter_indices

  type ( rhyme_plotter_indices ), parameter :: plid = rhyme_plotter_indices()


  type plotter_canvas_t
    integer :: x = plid%unset, y = plid%unset
    real ( kind=8 ) :: dx(4), minmax(2, 4)
    logical :: added_axis(4) = .false.
    integer :: axis_scale(4) = plid%linear, nticks(4) = 0, n_pixels(4) = 0
    character ( len=32, kind=plid%ucs4 ), allocatable :: table(:, :)
  contains
    procedure :: init => rhyme_plotter_canvas_init
    procedure :: add_axis => rhyme_plotter_canvas_add_axis
    procedure :: plot => rhyme_plotter_canvas_plot
  end type plotter_canvas_t


  interface
    pure module subroutine rhyme_plotter_canvas_init ( this, x, y )
      class ( plotter_canvas_t ), intent ( inout ) :: this
      integer, intent ( in ) :: x, y
    end subroutine rhyme_plotter_canvas_init

    module subroutine rhyme_plotter_canvas_add_axis ( this, pos, nticks, &
      minmax, scale, label )
      class ( plotter_canvas_t ), intent ( inout ) :: this
      integer, intent ( in ) :: pos, nticks
      real ( kind=8 ), intent ( in ) :: minmax(2)
      integer, intent ( in ), optional :: scale
      character ( len=* ), intent ( in ), optional :: label
    end subroutine rhyme_plotter_canvas_add_axis

    module subroutine rhyme_plotter_canvas_plot ( this )
      class ( plotter_canvas_t ), intent ( inout ) :: this
    end subroutine rhyme_plotter_canvas_plot
  end interface
end module rhyme_plotter
