module rhyme_initial_condition
  use rhyme_samr
  use rhyme_chombo
  use rhyme_log

  implicit none

  type initial_condition_indices_t
    integer :: unset = -1
    integer :: simple = 1, load = 2
  end type initial_condition_indices_t

  type ( initial_condition_indices_t ), parameter :: icid = initial_condition_indices_t ()


  type initial_condition_t
    integer :: type = icid%unset
    integer :: base_grid(3) = icid%unset
    integer :: nlevels = icid%unset
    integer :: max_nboxes(0:samrid%max_nlevels) = 0
    character ( len=2024 ) :: path = ''
  contains
    procedure :: init => rhyme_initial_condition_init
    procedure :: init_simple => rhyme_initial_condition_init_simple
    procedure :: load => rhyme_initial_condition_load
  end type initial_condition_t

contains

  subroutine rhyme_initial_condition_init ( this, samr, log )
    implicit none

    class ( initial_condition_t ), intent ( in ) :: this
    type ( samr_t ), intent ( inout ) :: samr
    type ( log_t ), intent ( inout ) :: log

    if ( samr%initialized ) then
      call log%warn( 'Trying to re-initialize SAMR object')
      return
    end if

    if ( this%type .eq. icid%unset ) then
      call log%err( 'ic_type is not set' )
      return
    end if

    if ( all( this%max_nboxes < 1 ) ) then
      call log%err_kw1d( 'max_nboxes is not valid', 'max_nboxes', this%max_nboxes )
    end if

    if ( this%type .eq. icid%simple ) then
      call this%init_simple( samr, log )
    else if ( this%type .eq. icid%load ) then
      call this%load( samr, log )
    else
      call log%err_kw( 'Unknown initial condition type', 'ic_type', this%type )
      return
    end if

  end subroutine rhyme_initial_condition_init


  subroutine rhyme_initial_condition_init_simple ( this, samr, log )
    implicit none

    class ( initial_condition_t ), intent ( in ) :: this
    type ( samr_t ), intent ( inout ) :: samr
    type ( log_t ), intent ( inout ) :: log

    real ( kind=8 ) :: ref_factor
    integer :: l, lb(3), ub(3), stat

    if ( &
      any( this%base_grid .eq. icid%unset ) &
      .or. this%nlevels .eq. icid%unset ) then
      call log%err( 'ic_base_grid or ic_nlevels is not set' )
      return
    end if

    samr%nlevels = this%nlevels
    samr%base_grid = this%base_grid
    samr%ghost_cells = merge( 2, 0, samr%base_grid > 1 )
    samr%max_nboxes = this%max_nboxes
    samr%max_nboxes( samr%nlevels: ) = 0

    samr%levels%level = [ ( l, l=0, 23 ) ]
    samr%levels%nboxes = 0
    samr%levels%refine_factor = 2.d0
    samr%levels%max_nboxes = samr%max_nboxes

    do l = 1, samr%nlevels - 1
      samr%levels(l)%dx = merge ( &
        1.d0 / real( samr%base_grid, kind=8 ) / ref_factor(l)**l, &
        1.d0, &
        samr%base_grid .ne. 1 &
      )

      allocate( samr%levels(l)%boxes( samr%max_nboxes(l) ) )
    end do

    ! Initializing the first level
    samr%levels(0)%boxes(1)%dims = samr%base_grid
    samr%levels(0)%boxes(1)%left_edge = 1
    samr%levels(0)%boxes(1)%right_edge = samr%base_grid

    lb = -samr%ghost_cells + 1
    ub = samr%base_grid + samr%ghost_cells

    allocate ( samr%levels(0)%boxes(1)%hydro ( &
      lb(1):ub(1), lb(2):ub(2), lb(3):ub(3) &
    ), stat=stat )

    allocate ( samr%levels(0)%boxes(1)%flags ( &
      lb(1):ub(1), lb(2):ub(2), lb(3):ub(3) &
    ), stat=stat )

    samr%levels(0)%nboxes = 1

    samr%initialized = .true.
  end subroutine rhyme_initial_condition_init_simple


  subroutine rhyme_initial_condition_load ( this, samr, log )
    use rhyme_chombo

    implicit none

    class ( initial_condition_t ), intent ( in ) :: this
    type ( samr_t ), intent ( inout ) :: samr
    type ( log_t ), intent ( inout ) :: log

    type ( chombo_t ) :: chombo
    integer :: l
    character ( len=16 ) :: level_name
    logical :: exist

    inquire ( file=trim(this%path), exist=exist )
    if ( .not. exist ) then
      call log%err( 'ic_snap does not exist' )
      return
    end if

    call chombo%open( this%path )

    call chombo%read_group_attr( '/', 'num_level', samr%nlevels )
    call chombo%read_group_1d_array_attr( '/', 'ProblemDomain', samr%base_grid )

    do l = 0, samr%nlevels - 1
      write ( level_name, '(A7,I0)') "/level_", l

      ! TODO: open levels, read boxes, allocate and load samr boxes
    end do
    call chombo%close
  end subroutine rhyme_initial_condition_load
end module rhyme_initial_condition
