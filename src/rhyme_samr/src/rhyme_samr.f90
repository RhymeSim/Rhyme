module rhyme_samr
  use rhyme_hydro_base

  implicit none

  type samr_indices_t
    integer :: ghost = -1
    integer :: max_nlevels = 23
  end type samr_indices_t

  type ( samr_indices_t ), parameter :: samrid = samr_indices_t ()


  type samr_box_t
    integer :: dims(3)
    real(kind=8) :: left_edge(3), right_edge(3)
    integer, allocatable :: flags (:, :, :)
    type ( hydro_conserved_t ), allocatable :: hydro (:, :, :)
  end type samr_box_t


  type samr_level_t
    real(kind=8) :: refine_factor
    real(kind=8) :: t, dt, dx(3)
    integer :: nboxes, max_nboxes, iteration
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
    procedure :: init_with => rhyme_samr_init_with
    procedure :: init => rhyme_samr_init
    procedure :: init_box => rhyme_samr_init_box
  end type samr_t


  integer, parameter :: init_nboxes(0:samrid%max_nlevels) = [ &
    1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 &
  ]

contains

  !> Initialize a Structured AMR
  !> @param[in] base_grid Base grid dimensions
  !> @param[in] n_levels Number of refinement levels (not including the zeroth level)
  !> @param[in] max_nboxes Maximum number of boxes
  !> @param[in] ghost_cells Ghost cells (responsible for boundary condition)
  pure subroutine rhyme_samr_init_with ( this, base_grid, nlevels, max_nboxes, ghost_cells )
    implicit none

    class ( samr_t ), intent(inout) :: this
    integer, intent(in) :: max_nboxes(0:samrid%max_nlevels)
    integer, intent(in) :: base_grid(3), nlevels, ghost_cells(3)


    if ( this%initialized ) return

    this%nlevels = nlevels
    this%max_nboxes(:) = max_nboxes(:)
    this%ghost_cells(:) = ghost_cells(:)
    this%base_grid(:) = base_grid(:)

    call rhyme_samr_init ( this )

  end subroutine rhyme_samr_init_with


  pure subroutine rhyme_samr_init ( this )
    implicit none

    class ( samr_t ), intent(inout) :: this
    integer :: l, b, lb(3), ub(3)
    real ( kind=8 ) :: ref_factor ( 0:samrid%max_nlevels )

    if ( this%initialized ) return

    ref_factor = 2.d0
    this%max_nboxes ( this%nlevels: ) = 0


    do l = 0, this%nlevels - 1
      this%levels(l)%nboxes = init_nboxes(l)
      this%levels(l)%refine_factor = ref_factor(l)
      this%levels(l)%max_nboxes = this%max_nboxes(l)

      this%levels(l)%dx = merge ( &
        1.d0 / real( this%base_grid, kind=8 ) / ref_factor(l)**l, &
        1.d0, &
        this%base_grid .ne. 1 &
      )

      allocate ( this%levels(l)%boxes( this%max_nboxes(l) ) )

      if ( l == 0 ) then ! Initializing the first level
        do b = 1, 1 ! Initializing the only box of the first level
          this%levels(l)%boxes(b)%dims(:) = this%base_grid(:)
          this%levels(l)%boxes(b)%left_edge = 0.d0
          this%levels(l)%boxes(b)%right_edge = 1.d0

          lb = - this%ghost_cells + 1
          ub = this%base_grid + this%ghost_cells

          allocate ( this%levels(l)%boxes(b)%hydro ( &
            lb(1):ub(1), lb(2):ub(2), lb(3):ub(3) &
          ))

          allocate ( this%levels(l)%boxes(b)%flags ( &
            lb(1):ub(1), lb(2):ub(2), lb(3):ub(3) &
          ))
        end do
      end if
    end do

    this%initialized = .true.
  end subroutine rhyme_samr_init


  subroutine rhyme_samr_init_box ( this, l, b, dims, ledges, redges )
    ! NB: This subroutine allocates a box at a given location and increments
    !     the number of boxes on that level. Make sure all the boxes between
    !     box 1 to box b in the level are allocated!
    !     Also the subroutine does not check if the box has already been
    !     allocated or not, very unsafe, I know ;)
    implicit none

    class ( samr_t ), intent ( inout ) :: this
    integer, intent ( in ) :: l, b, dims(3)
    real ( kind=8 ), intent ( in ) :: ledges(3), redges(3)

    integer :: lb(3), ub(3)

    lb = - this%ghost_cells + 1
    ub = dims + this%ghost_cells

    allocate ( this%levels(l)%boxes(b)%flags ( &
      lb(1):ub(1), lb(2):ub(2), lb(3):ub(3) &
    ) )

    allocate ( this%levels(l)%boxes(b)%hydro ( &
      lb(1):ub(1), lb(2):ub(2), lb(3):ub(3) &
    ) )

    this%levels(l)%nboxes = this%levels(l)%nboxes + 1

    this%levels(l)%boxes(b)%left_edge(:) = ledges(:)
    this%levels(l)%boxes(b)%right_edge(:) = redges(:)
  end subroutine rhyme_samr_init_box
end module rhyme_samr
