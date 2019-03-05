module rhyme_samr
  use rhyme_hydro_base

  implicit none

  type samr_indices_t
    integer :: ghost = -1
    integer :: unset = -10
    integer :: max_nlevels = 23
  end type samr_indices_t

  type ( samr_indices_t ), parameter :: samrid = samr_indices_t ()


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
    integer :: nboxes = samrid%unset
    integer :: max_nboxes = samrid%unset
    integer :: iteration = samrid%unset
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
    integer :: max_nboxes (0:samrid%max_nlevels)
    type ( samr_level_t ) :: levels(0:samrid%max_nlevels)
  contains
    procedure :: init_box => rhyme_samr_init_box
  end type samr_t

contains
  subroutine rhyme_samr_init_box ( this, l, b, dims, ledges, redges )
    ! NB: This subroutine allocates a box at a given location and increments
    !     the number of boxes on that level. Make sure all the boxes between
    !     box 1 to box b in the level are allocated!
    !     Also the subroutine does not check if the box has already been
    !     allocated or not, very unsafe, I know ;)
    implicit none

    class ( samr_t ), intent ( inout ) :: this
    integer, intent ( in ) :: l, b, dims(3)
    integer, intent ( in ) :: ledges(3), redges(3)

    integer :: lb(3), ub(3)

    this%levels(l)%boxes(b)%level = l
    this%levels(l)%boxes(b)%number = b

    lb = -this%ghost_cells + 1
    ub = dims + this%ghost_cells

    allocate ( this%levels(l)%boxes(b)%flags ( &
      lb(1):ub(1), lb(2):ub(2), lb(3):ub(3) &
    ) )

    allocate ( this%levels(l)%boxes(b)%hydro ( &
      lb(1):ub(1), lb(2):ub(2), lb(3):ub(3) &
    ) )

    this%levels(l)%nboxes = this%levels(l)%nboxes + 1

    this%levels(l)%boxes(b)%dims = dims
    this%levels(l)%boxes(b)%left_edge(:) = ledges(:)
    this%levels(l)%boxes(b)%right_edge(:) = redges(:)
  end subroutine rhyme_samr_init_box
end module rhyme_samr
