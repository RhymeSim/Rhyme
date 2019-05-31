module rhyme_samr
  use rhyme_hydro_base

  implicit none

  type samr_indices_t
    integer :: ghost = -1
    integer :: unset = -10
    integer :: max_nlevels = 23
    integer :: left = 1, right = 2
#if NDIM > 1
    integer :: bottom = 3, top = 4
#endif
#if NDIM > 2
    integer :: back = 5, front = 6
#endif
  end type samr_indices_t

  type ( samr_indices_t ), parameter :: samrid = samr_indices_t()


  type samr_box_t
    integer :: dims( NDIM ) = samrid%unset
    integer :: level = samrid%unset
    integer :: number = samrid%unset
    integer :: left_edge( NDIM ) = samrid%unset
    integer :: right_edge( NDIM ) = samrid%unset
#if NDIM == 1
    integer, allocatable :: flags (:)
    real ( kind=8 ), allocatable :: cells (:, :)
#elif NDIM == 2
    integer, allocatable :: flags (:, :)
    real ( kind=8 ), allocatable :: cells (:, :, :)
#elif NDIM == 3
    integer, allocatable :: flags (:, :, :)
    real ( kind=8 ), allocatable :: cells (:, :, :, :)
#endif
  end type samr_box_t


  type samr_level_t
    integer :: level = samrid%unset
    integer :: nboxes = 0
    integer :: max_nboxes = 0
    integer :: iteration = 0
    real ( kind=8 ) :: refine_factor
    real ( kind=8 ) :: t = 0.d0
    real ( kind=8 ) :: dt, dx( NDIM )
    type ( samr_box_t ), allocatable :: boxes(:)
  end type samr_level_t


  type samr_t
    logical :: initialized = .false.
    integer :: nlevels
    integer :: base_grid( NDIM )
    integer :: ghost_cells( NDIM )
    integer :: max_nboxes ( 0:samrid%max_nlevels )
    real ( kind=8 ) :: box_lengths( NDIM ) ! In code units, look at rhyme_units module
    type ( samr_level_t ) :: levels( 0:samrid%max_nlevels )
  contains
    procedure :: init_box => rhyme_samr_init_box
  end type samr_t


  interface
    module subroutine rhyme_samr_init_box ( this, l, b, dims, ledges, redges )
      class ( samr_t ), intent ( inout ) :: this
      integer, intent ( in ) :: l, b, dims( NDIM )
      integer, intent ( in ) :: ledges( NDIM ), redges( NDIM )
    end subroutine rhyme_samr_init_box
  end interface
end module rhyme_samr
