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
    character ( len=1024 ) :: snapshot_path = ''
  contains
    procedure :: init => rhyme_initial_condition_init
    procedure :: init_simple => rhyme_initial_condition_init_simple
    procedure :: load_snapshot => rhyme_initial_condition_load_snapshot
    procedure :: load_headers => rhyme_initial_condition_load_headers
    procedure :: load_rhyme => rhyme_initial_condition_load_rhyme
    procedure :: load_r2c_2d => rhyme_initial_condition_load_r2c_2d
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
      call log%err( 'max_nboxes is not valid', 'max_nboxes', '=', this%max_nboxes )
    end if

    if ( this%type .eq. icid%simple ) then
      call this%init_simple( samr, log )
    else if ( this%type .eq. icid%snapshot ) then
      call this%load_snapshot( samr, ig, log )
    else
      call log%err( 'Unknown initial condition type', 'ic_type', '=', [ this%type ] )
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
    samr%levels(0)%boxes(1)%level = 0
    samr%levels(0)%boxes(1)%number = 1
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

    logical :: exist

    inquire ( file=trim(this%snapshot_path), exist=exist )
    if ( .not. exist ) then
      call log%err( 'Snapshot does not exist', 'snapshot_path', '=', [ this%snapshot_path ] )
      return
    end if

    call this%load_headers( samr )

    samr%levels%nboxes = 0 ! It will be incremented by init_box procedure

    select case ( this%snapshot_type )
    case ( icid%rhyme )
      call this%load_rhyme( samr, log )
    case ( icid%r2c_2d )
      call this%load_r2c_2d( samr, ig, log )
    case default
      call log%err( 'Unsupported snapshot format', &
        'snapshot_type', '=', [ this%snapshot_type ] )
    end select

    samr%initialized = .true.
  end subroutine rhyme_initial_condition_load_snapshot


  subroutine rhyme_initial_condition_load_headers ( this, samr )
    implicit none

    class ( initial_condition_t ), intent ( in ) :: this
    type ( samr_t ), intent ( inout ) :: samr

    type ( chombo_t ) :: ch
    integer :: l, prob_domain(3)

    call ch%open( this%snapshot_path )

    call ch%read_group_attr( '/', 'num_levels', samr%nlevels )
    call ch%read_group_attr( '/', 'iteration', samr%levels(0)%iteration )
    call ch%read_group_attr( '/', 'time', samr%levels(0)%t )
    call ch%read_group_comp_1d_array_attr( &
      'level_0', 'prob_domain', icid%prob_domain_headers, prob_domain )
    samr%base_grid = prob_domain + 1
    samr%ghost_cells = merge( 2, 0, samr%base_grid > 1 )

    ! Initialize other variables
    samr%max_nboxes = this%max_nboxes
    samr%levels%max_nboxes = this%max_nboxes
    samr%levels%level = [ (l, l=0, samrid%max_nlevels) ]

    do l = 0, samr%nlevels - 1
      samr%levels(l)%dx = 1.d0 / ( samr%base_grid * 2.d0**l )
    end do

    call ch%close
  end subroutine rhyme_initial_condition_load_headers


  subroutine rhyme_initial_condition_load_rhyme ( this, samr, log )
    implicit none

    class ( initial_condition_t ), intent ( in ) :: this
    type ( samr_t ), intent ( inout ) :: samr
    type ( log_t ), intent ( inout ) :: log

    integer, parameter :: ncomp = 5

    type ( chombo_t ) :: ch
    integer :: l, b, ofs
    integer :: nboxes, lboxes
    integer :: bdims(3), ub(3), blen
    character ( len=16 ) :: level_name
    integer, allocatable :: boxes(:,:)
    real ( kind=8 ), allocatable :: data(:)

    call ch%open( this%snapshot_path )

    do l = 0, samr%nlevels - 1
      write ( level_name, '(A7,I0)') "/level_", l

      allocate( samr%levels(l)%boxes( samr%levels(l)%max_nboxes ) )

      nboxes = ch%get_table_size( trim(level_name)//'/boxes' )
      allocate( boxes( 6, nboxes ) )

      call ch%read_table( trim(level_name), 'boxes', icid%boxes_headers, boxes )

      if ( nboxes > samr%levels(l)%max_nboxes ) then
        call log%err( 'Number of boxes is less than maximum available', &
          nboxes, '>', [ samr%levels(l)%max_nboxes ] )
        return
      end if

      lboxes = sum( [ (product(boxes(4:6, b) + 1), b=1, nboxes ) ] )

      ! Reading data dataset
      allocate( data( ncomp * lboxes ) )
      call ch%read_1d_dataset( trim(level_name)//'/data:datatype=0', data )

      ofs = 0
      do b = 1, nboxes
        samr%levels(l)%boxes(b)%level = l
        samr%levels(l)%boxes(b)%number = b

        bdims = boxes(4:6, b) - boxes(1:3, b) + 1
        blen = product( bdims )
        ub = bdims

        call samr%init_box( l, b, bdims, boxes(1:3, b) + 1, boxes(4:6, b) + 1 )

        samr%levels(l)%boxes(b)%hydro(1:ub(1),1:ub(2),1:ub(3))%u(hyid%rho) = &
        reshape( data( ofs+0*blen+1:ofs+1*blen ), bdims )

        samr%levels(l)%boxes(b)%hydro(1:ub(1),1:ub(2),1:ub(3))%u(hyid%rho_u) = &
        reshape( data( ofs+1*blen+1:ofs+2*blen ), bdims )

        samr%levels(l)%boxes(b)%hydro(1:ub(1),1:ub(2),1:ub(3))%u(hyid%rho_v) = &
        reshape( data( ofs+2*blen+1:ofs+3*blen ), bdims )

        samr%levels(l)%boxes(b)%hydro(1:ub(1),1:ub(2),1:ub(3))%u(hyid%rho_w) = &
        reshape( data( ofs+3*blen+1:ofs+4*blen ), bdims )

        samr%levels(l)%boxes(b)%hydro(1:ub(1),1:ub(2),1:ub(3))%u(hyid%e_tot) = &
        reshape( data( ofs+4*blen+1:ofs+5*blen ), bdims )

        ofs = ofs + ncomp * blen
      end do

      deallocate( data )
      deallocate( boxes )
    end do

    call ch%close
  end subroutine rhyme_initial_condition_load_rhyme


  subroutine rhyme_initial_condition_load_r2c_2d ( this, samr, ig, log )
    implicit none

    class ( initial_condition_t ), intent ( in ) :: this
    type ( samr_t ), intent ( inout ) :: samr
    type ( ideal_gas_t ), intent ( in ) :: ig
    type ( log_t ), intent ( inout ) :: log


    type ( chombo_t ) :: ch
    integer :: l, b, ofs
    integer :: nboxes, lboxes
    integer :: bdims(3), ub(3), blen
    character ( len=16 ) :: level_name
    integer, allocatable :: boxes(:,:)
    real ( kind=8 ), allocatable :: data(:)
    real ( kind=8 ) :: rho_b
    real ( kind=8 ) :: scale_l, scale_t, scale_d, scale_p
    integer:: ncomp

    ! Fix wrong grid dimension (buf in R2C)
    samr%base_grid(3) = 1
    samr%ghost_cells(3) = 0

    call ch%open( this%snapshot_path )

    call ch%read_group_attr( '/', 'MeanBarDen', rho_b )
    call ch%read_group_attr( '/', 'scale_l', scale_l )
    call ch%read_group_attr( '/', 'scale_t', scale_t )
    call ch%read_group_attr( '/', 'scale_d', scale_d )
    call ch%read_group_attr( '/', 'num_components', ncomp )

    scale_p = scale_d * ( scale_l / scale_t )**2

    do l = 0, samr%nlevels - 1
      write ( level_name, '(A7,I0)') "/level_", l

      allocate( samr%levels(l)%boxes( samr%levels(l)%max_nboxes ) )

      nboxes = ch%get_table_size( trim(level_name)//'/boxes' )
      allocate( boxes( 6, nboxes ) )

      call ch%read_table( trim(level_name), 'boxes', icid%boxes_headers, boxes )

      if ( nboxes > samr%levels(l)%max_nboxes ) then
        call log%err( 'Number of boxes is less than maximum available', &
          nboxes, '>', [ samr%levels(l)%max_nboxes ] )
        return
      end if

      lboxes = sum( [ (product(boxes(4:6, b) + 1), b=1, nboxes ) ] )

      ! Reading data dataset
      allocate( data( ncomp * lboxes ) )
      call ch%read_1d_dataset( trim(level_name)//'/data:datatype=0', data )

      ofs = 0
      do b = 1, nboxes
        bdims = boxes(4:6, b) - boxes(1:3, b) + 1
        blen = product( bdims )
        ub = bdims

        call samr%init_box( l, b, bdims, boxes(1:3, b) + 1, boxes(4:6, b) + 1 )

        samr%levels(l)%boxes(b)%hydro(1:ub(1),1:ub(2),1:ub(3))%u(hyid%rho) = &
          reshape( scale_d * data( 1:product(bdims) ), bdims )

        samr%levels(l)%boxes(b)%hydro(1:ub(1),1:ub(2),1:ub(3))%u(hyid%rho_u) = &
          reshape( scale_d * data( 1:blen ) * scale_l / scale_t * data( 1*blen+1:2*blen ), bdims )

        samr%levels(l)%boxes(b)%hydro(1:ub(1),1:ub(2),1:ub(3))%u(hyid%rho_v) = &
          reshape( scale_d * data( 1:blen ) * scale_l / scale_t * data( 2*blen+1:3*blen ), bdims )

        samr%levels(l)%boxes(b)%hydro(1:ub(1),1:ub(2),1:ub(3))%u(hyid%rho_w) = 0.d0

        samr%levels(l)%boxes(b)%hydro(1:ub(1),1:ub(2),1:ub(3))%u(hyid%e_tot) = &
          reshape( &
            .5d0 * scale_d * data( 1:blen ) * (scale_l / scale_t)**2 * ( &
              data( 1*blen+1:2*blen )**2 + data( 2*blen+1:3*blen )**2 &
            ) + scale_p * data( 3*blen+1:4*blen ) / ( ig%gamma - 1.d0 ), &
          bdims )

        ofs = ofs + ncomp * blen
      end do

      deallocate( data )
      deallocate( boxes )
    end do

    call ch%close
  end subroutine rhyme_initial_condition_load_r2c_2d
end module rhyme_initial_condition
