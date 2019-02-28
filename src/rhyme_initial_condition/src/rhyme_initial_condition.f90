module rhyme_initial_condition
  use rhyme_samr
  use rhyme_chombo
  use rhyme_ideal_gas
  use rhyme_log

  implicit none

  type initial_condition_indices_t
    integer :: unset = -1
    integer :: simple = 1, snapshot = 2
    integer :: rhyme = 10, radamesh = 11, r2c_2d = 12
    character ( len=8 ), dimension(3) :: prob_domain_headers = [ &
      'hi_i    ', 'hi_j    ', 'hi_k    ' ]
    character ( len=8 ) :: boxes_headers(6) = [ &
      'lo_i    ', 'lo_j    ', 'lo_k    ', 'hi_i    ', 'hi_j    ', 'hi_k    ' ]
  end type initial_condition_indices_t

  type ( initial_condition_indices_t ), parameter :: icid = initial_condition_indices_t ()


  type initial_condition_t
    integer :: type = icid%unset
    integer :: snapshot_type = icid%unset
    integer :: nlevels = icid%unset
    integer :: base_grid(3) = icid%unset
    integer :: max_nboxes(0:samrid%max_nlevels) = 0
    character ( len=1024 ) :: path = ''
  contains
    procedure :: init => rhyme_initial_condition_init
    procedure :: init_simple => rhyme_initial_condition_init_simple
    procedure :: load_snapshot => rhyme_initial_condition_load_snapshot
  end type initial_condition_t

contains

  subroutine rhyme_initial_condition_init ( this, samr, ig, log )
    implicit none

    class ( initial_condition_t ), intent ( in ) :: this
    type ( samr_t ), intent ( inout ) :: samr
    type ( ideal_gas_t ), intent ( in ) :: ig
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
    else if ( this%type .eq. icid%snapshot ) then
      call this%load_snapshot( samr, ig, log )
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

    integer :: l, lb(3), ub(3), stat

    if ( any( this%base_grid .eq. icid%unset ) &
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

    do l = 0, samr%nlevels - 1
      samr%levels(l)%dx = merge ( &
        1.d0 / real( samr%base_grid, kind=8 ) / 2.d0**l, &
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


  subroutine rhyme_initial_condition_load_snapshot ( this, samr, ig, log )
    use rhyme_chombo

    implicit none

    class ( initial_condition_t ), intent ( in ) :: this
    type ( samr_t ), intent ( inout ) :: samr
    type ( ideal_gas_t ), intent ( in ) :: ig
    type ( log_t ), intent ( inout ) :: log

    type ( chombo_t ) :: chombo
    integer :: l, b, ub(3), boxes_size, prob_domain(3), box_dims(3), b_len
    integer, allocatable :: boxes(:,:)
    real ( kind=8 ), allocatable :: data(:)
    character ( len=16 ) :: level_name
    logical :: exist

    inquire ( file=trim(this%path), exist=exist )
    if ( .not. exist ) then
      call log%err( 'ic_snap does not exist' )
      return
    end if

    call chombo%open( this%path )

    call chombo%read_group_attr( '/', 'num_levels', samr%nlevels )
    call chombo%read_group_comp_1d_array_attr( &
      'level_0', 'prob_domain', icid%prob_domain_headers, prob_domain )
    samr%base_grid = prob_domain + 1
    samr%ghost_cells = merge( 2, 0, samr%base_grid > 1 )
    samr%max_nboxes = this%max_nboxes

    do l = 0, samr%nlevels - 1
      write ( level_name, '(A7,I0)') "/level_", l

      samr%levels(l)%max_nboxes = this%max_nboxes(l)
      allocate( samr%levels(l)%boxes( samr%levels(l)%max_nboxes ) )

      boxes_size = chombo%get_table_size( trim(level_name)//'/boxes' )
      allocate( boxes( 6, boxes_size ) )

      call chombo%read_table( trim(level_name), 'boxes', icid%boxes_headers, boxes )

      if ( boxes_size > samr%levels(l)%max_nboxes ) then
        call log%err_kw( 'Number of boxes is less than maximum available', &
          boxes_size, samr%levels(l)%max_nboxes, '>' )
        return
      end if

      do b = 1, boxes_size
        box_dims = boxes(4:6, b) - boxes(1:3, b) + 1
        b_len = product( box_dims )
        ub = box_dims

        call samr%init_box( l, b, box_dims, boxes(1:3, b) + 1, boxes(4:6, b) + 1 )

        select case ( this%snapshot_type )
        case ( icid%r2c_2d )
          allocate( data(7 * product( box_dims )) )
          call chombo%read_1d_dataset( trim(level_name)//'/data:datatype=0', data )

          samr%levels(l)%boxes(b)%hydro(1:ub(1),1:ub(2),1:ub(3))%u(hyid%rho) = &
            reshape( data( 1:product(box_dims) ), box_dims )

          samr%levels(l)%boxes(b)%hydro(1:ub(1),1:ub(2),1:ub(3))%u(hyid%rho_u) = &
            reshape( data( 1:b_len ) * data( 1*b_len+1:2*b_len ), box_dims )

          samr%levels(l)%boxes(b)%hydro(1:ub(1),1:ub(2),1:ub(3))%u(hyid%rho_v) = &
            reshape( data( 1:b_len ) * data( 2*b_len+1:3*b_len ), box_dims )

          samr%levels(l)%boxes(b)%hydro(1:ub(1),1:ub(2),1:ub(3))%u(hyid%rho_v) = 0.d0

          samr%levels(l)%boxes(b)%hydro(1:ub(1),1:ub(2),1:ub(3))%u(hyid%rho_v) = &
            reshape( &
              0.d5 * data( 1:b_len ) * ( &
                data( 1*b_len+1:2*b_len )**2 + data( 2*b_len+1:3*b_len )**2 &
              ) + data( 3*b_len+1:4*b_len ) / ( ig%gamma - 1.d0 ) &
            , box_dims )
          deallocate( data )
        case default
          call log%err_kw( 'Unsupported snapshot format', 'snapshot_type', this%snapshot_type )
        end select
      end do

      deallocate( boxes )
    end do

    call chombo%close
  end subroutine rhyme_initial_condition_load_snapshot
end module rhyme_initial_condition
