module rhyme_samr
  use rhyme_hydro_base


  implicit none


  type samr_indices_t
    integer :: ghost = -1
  end type samr_indices_t

  type ( samr_indices_t ), parameter :: samrid = samr_indices_t ()


  type samr_box_t
    integer :: dims(3)
    integer, allocatable :: flags (:, :, :)
    type ( hydro_conserved_t ), allocatable :: hydro (:, :, :)
    real(kind=8) :: left_edge(3), right_edge(3)
  end type samr_box_t


  type refine_lev_t
    real(kind=8) :: refine_factor
    real(kind=8) :: t, dt, dx(3)
    integer :: nboxes, tot_nboxes
    type ( samr_box_t ), allocatable :: boxes(:)
  end type refine_lev_t


  type samr_t
    type ( refine_lev_t ), allocatable :: levels(:)
    integer :: nlevels
    integer :: base_grid(3)
    integer :: ghost_cells(3)
    integer :: tot_nboxes (0:23)
    logical :: initialized = .false.
  contains
    procedure :: init_with => init_samr_with
    procedure :: init => init_samr
  end type samr_t

contains

  !> Initialize a Structured AMR
  !> @param[in] dims Base grid dimensions
  !> @param[in] n_levels Number of refinement levels (not including the zeroth level)
  !> @param[in] max_nboxes Maximum number of boxes
  !> @param[in] ghost_cells Ghost cells (responsible for boundary condition)
  pure subroutine init_samr_with ( this, dims, nlevels, tot_nboxes, ghost_cells )
    implicit none

    class ( samr_t ), intent(inout) :: this
    integer, intent(in) :: tot_nboxes(0:23)
    integer, intent(in) :: dims(3), nlevels, ghost_cells(3)


    if ( this%initialized ) return

    this%nlevels = nlevels
    this%tot_nboxes(0:23) = tot_nboxes(0:23)
    this%tot_nboxes(nlevels:) = 0
    this%ghost_cells = ghost_cells
    this%base_grid = dims

    call init_samr ( this )

  end subroutine init_samr_with

  pure subroutine init_samr ( this )
    implicit none

    class ( samr_t ), intent(inout) :: this
    integer :: i

    if ( this%initialized ) return

    allocate ( this%levels(0:this%nlevels-1) )

    this%tot_nboxes ( this%nlevels: ) = 0

    this%levels(0)%dx = 1.d0 / real ( this%base_grid, kind=8 )
    this%levels(0)%refine_factor = 1.d0
    this%levels(0)%tot_nboxes = this%tot_nboxes(0)

    this%levels(0)%nboxes = 1
    if ( .not. allocated ( this%levels(0)%boxes ) ) then
      allocate ( this%levels(0)%boxes ( this%tot_nboxes(0) ) )
    end if
    this%levels(0)%boxes(1)%dims(:) = this%base_grid(:)
    this%levels(0)%boxes(1)%left_edge = 0.0
    this%levels(0)%boxes(1)%right_edge = 1.0

    allocate( this%levels(0)%boxes(1)%flags( &
      -this%ghost_cells(1) + 1 : this%base_grid(1) + this%ghost_cells(1), &
      -this%ghost_cells(2) + 1 : this%base_grid(2) + this%ghost_cells(2), &
      -this%ghost_cells(3) + 1 : this%base_grid(3) + this%ghost_cells(3) &
    ) )

    allocate( this%levels(0)%boxes(1)%hydro( &
      -this%ghost_cells(1) + 1 : this%base_grid(1) + this%ghost_cells(1), &
      -this%ghost_cells(2) + 1 : this%base_grid(2) + this%ghost_cells(2), &
      -this%ghost_cells(3) + 1 : this%base_grid(3) + this%ghost_cells(3) &
    ) )

    if ( this%nlevels > 1 ) then
      do i = 1, this%nlevels - 1
        this%levels(i)%tot_nboxes = this%tot_nboxes(i)
        allocate ( this%levels(i)%boxes ( this%tot_nboxes(i) ) )
        this%levels(i)%nboxes = 0
        this%levels(i)%refine_factor = this%levels(i-1)%refine_factor / 2.d0
        this%levels(i)%dx = this%levels(i-1)%dx / 2.d0
        this%levels(i)%dx = merge ( 1.d0, this%levels(i)%dx, abs ( this%levels(i-1)%dx - 1.d0 ) < epsilon(0.d0))
      end do
    end if

    this%tot_nboxes ( this%nlevels: ) = 0

    this%initialized = .true.
  end subroutine init_samr
end module rhyme_samr
