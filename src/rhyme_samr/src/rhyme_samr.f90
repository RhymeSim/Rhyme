module rhyme_samr
  use rhyme_hydro_base

  implicit none

  type samr_indices_t
    integer :: ghost = -1
    integer :: unset = -10
    integer :: max_nlevels = 23
    integer :: left = 1, right = 2, bottom = 3, top = 4, back = 5, front = 6
  end type samr_indices_t

  type ( samr_indices_t ), parameter :: samrid = samr_indices_t()


  type samr_box_t
    integer :: dims(3) = samrid%unset
    integer :: level = samrid%unset
    integer :: number = samrid%unset
    integer :: left_edge(3) = samrid%unset
    integer :: right_edge(3) = samrid%unset
    integer, allocatable :: flags (:, :, :)
    type ( hydro_conserved_t ), allocatable :: hydro (:, :, :)
  end type samr_box_t


  type samr_level_t
    integer :: level = samrid%unset
    integer :: nboxes = 0
    integer :: max_nboxes = 0
    integer :: iteration = 0
    real ( kind=8 ) :: refine_factor
    real ( kind=8 ) :: t = 0.d0
    real ( kind=8 ) :: dt, dx(3)
    type ( samr_box_t ), allocatable :: boxes(:)
  end type samr_level_t


  type samr_t
    logical :: initialized = .false.
    integer :: nlevels
    integer :: base_grid(3)
    integer :: ghost_cells(3)
    integer :: max_nboxes ( 0:samrid%max_nlevels )
    real ( kind=8 ) :: box_lengths(3) ! In code units, look at rhyme_units module
    type ( samr_level_t ) :: levels( 0:samrid%max_nlevels )
  contains
    procedure :: init_box => rhyme_samr_init_box
  end type samr_t


  interface
    module subroutine rhyme_samr_init_box ( this, l, b, dims, ledges, redges )
      class ( samr_t ), intent ( inout ) :: this
      integer, intent ( in ) :: l, b, dims(3)
      integer, intent ( in ) :: ledges(3), redges(3)
    end subroutine rhyme_samr_init_box
  end interface
end module rhyme_samr
