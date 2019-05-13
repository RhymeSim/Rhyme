module rhyme_muscl_hancock_advection_factory
  use, intrinsic :: ieee_arithmetic
  use rhyme_muscl_hancock_factory
  use rhyme_samr_bc_factory
  use rhyme_assertion

  implicit none

  type rhyme_muscl_hancock_advection_factory_err_t
    logical :: is_bg = .false., failed = .false.
    integer :: i, j, k, axis
  end type rhyme_muscl_hancock_advection_factory_err_t


  type rhyme_muscl_hancock_advection_factory_t
    integer :: ngrids = 8
    integer :: slab_min_i = 1, slab_max_i = 1
    integer :: nlevels = 1
    integer :: max_nboxes( 0:samrid%max_nlevels ) = 0
    integer :: init_nboxes( 0:samrid%max_nlevels ) = 0
    real ( kind=8 ) :: courant_number = .8d0
    real ( kind=8 ) :: dx = 0.d0, dt = 0.d0, v = 0.d0
    type ( hydro_conserved_t ) :: slab, bg
    type ( rhyme_muscl_hancock_advection_factory_err_t ) :: err
    logical :: initialized = .false.
  contains
    procedure :: init => rhyme_muscl_hancock_advection_factory_init
    procedure :: set_ic_x => rhyme_muscl_hancock_advection_factory_set_ic_x
    procedure :: test_x => rhyme_muscl_hancock_advection_factory_test_x
    procedure :: set_ic_y => rhyme_muscl_hancock_advection_factory_set_ic_y
    procedure :: test_y => rhyme_muscl_hancock_advection_factory_test_y
    procedure :: set_ic_z => rhyme_muscl_hancock_advection_factory_set_ic_z
    procedure :: test_z => rhyme_muscl_hancock_advection_factory_test_z
    procedure :: handle_err => rhyme_muscl_hancock_advection_factory_handle_err
  end type rhyme_muscl_hancock_advection_factory_t

contains

  subroutine rhyme_muscl_hancock_advection_factory_init ( this, cfl, ig, axis )
    implicit none

    class ( rhyme_muscl_hancock_advection_factory_t ), intent ( inout ) :: this
    type ( cfl_t ), intent ( in ) :: cfl
    type ( ideal_gas_t ), intent ( in ) :: ig
    integer, intent ( in ) :: axis

    type ( hydro_primitive_t ) :: slab_prim, bg_prim
    integer :: idx

    slab_prim%w = [ 1.d0, 0.d0, 0.d0, 0.d0, .1d0 ]
    bg_prim%w = [ 1.d0, 0.d0, 0.d0, 0.d0, .1d0 ]

    this%ngrids = 8
    this%slab_min_i = this%ngrids / 4
    this%slab_max_i = this%ngrids * 3 / 4

    this%nlevels = 1

    this%max_nboxes = 0
    this%max_nboxes( 0:0 ) = [ 1 ]

    this%init_nboxes = 0
    this%init_nboxes( 0:0 ) = [ 1 ]

    this%courant_number = cfl%courant_number

    this%dx = 1.d0 / this%ngrids

    call ig%prim_to_cons( bg_prim, this%bg )
    this%dt = this%courant_number * this%dx / ig%p( this%bg )

    this%v = this%dx / this%dt

    select case ( axis )
    case ( hyid%x ); idx = hyid%u
    case ( hyid%y ); idx = hyid%v
    case ( hyid%z ); idx = hyid%w
    case default
      print *, 'Wrong axis'
      this%err%failed = .true.
      stop
    end select

    slab_prim%w( idx ) = this%v
    call ig%prim_to_cons( slab_prim, this%slab )

    this%initialized = .true.
  end subroutine rhyme_muscl_hancock_advection_factory_init


  subroutine rhyme_muscl_hancock_advection_test ( solver, ws, axis, tester )
    use rhyme_cfl_factory
    use rhyme_ideal_gas_factory
    use rhyme_samr_factory
    use rhyme_samr_bc_factory
    use rhyme_irs_factory
    use rhyme_slope_limiter_factory
    use rhyme_chombo_factory
    use rhyme_log_factory

    implicit none

    interface
      subroutine solver ( cfg, box, dx, dt, cfl, ig, irs, sl, ws )
        use rhyme_muscl_hancock

        class ( muscl_hancock_t ), intent ( inout ) :: cfg
        type ( samr_box_t ), intent ( inout ) :: box
        real ( kind=8 ), intent ( in ) :: dx(3), dt
        type ( cfl_t ), intent ( in ) :: cfl
        type ( ideal_gas_t ), intent ( in ) :: ig
        type ( irs_t ), intent ( inout ) :: irs
        type ( slope_limiter_t ), intent ( in ) :: sl
        type ( mh_workspace_t ), intent ( inout ) :: ws
      end subroutine solver
    end interface
    type ( mh_workspace_t ), intent ( inout ) :: ws
    integer, intent ( in ) :: axis
    type ( assertion_t ), intent ( inout ) :: tester

    type ( muscl_hancock_t ) :: mh
    type ( cfl_t ) :: cfl
    type ( ideal_gas_t ) :: ig
    type ( samr_t ) :: samr
    type ( samr_bc_t ) :: bc
    type ( irs_t ) :: irs
    type ( slope_limiter_t ) :: sl
    type ( chombo_t ) :: chombo
    type ( log_t ) :: logger

    type ( rhyme_muscl_hancock_advection_factory_t ) :: adv
    integer :: step

    cfl = cfl_factory%generate()
    ig = ig_factory%generate()
    samr = samr_factory%generate( physical=.true. )
    bc = bc_factory%generate()
    irs = irs_factory%generate()
    sl = sl_factory%generate()
    chombo = ch_factory%generate()
    logger = log_factory%generate()

    call adv%init( cfl, ig, axis )
    call chombo%init( logger )


    ! Setting up samr
    select case ( axis )
    case ( hyid%x ); call adv%set_ic_x( samr, bc, logger )
    case ( hyid%y ); call adv%set_ic_y( samr, bc, logger )
    case ( hyid%z ); call adv%set_ic_z( samr, bc, logger )
    end select

    ! Initializing MH and WS object
    call rhyme_muscl_hancock_init( mh, samr, ws, logger )

    do step = 1, 119 * adv%ngrids
      call bc%set_base_grid_boundaries( samr )

      call solver( mh, samr%levels(0)%boxes(1), &
        samr%levels(0)%dx, &
        adv%dt, cfl, ig, irs, sl, ws )

      call chombo%write_samr( samr )

      ! Test
      select case ( axis )
      case ( hyid%x )
        call adv%test_x( samr%levels(0)%boxes(1) )
        if ( adv%err%failed ) adv%err%axis = hyid%x
      case ( hyid%y )
        call adv%test_y( samr%levels(0)%boxes(1) )
        if ( adv%err%failed ) adv%err%axis = hyid%y
      case ( hyid%z )
        call adv%test_z( samr%levels(0)%boxes(1) )
        if ( adv%err%failed ) adv%err%axis = hyid%z
      end select

      samr%levels(0)%iteration = samr%levels(0)%iteration + 1

      if ( adv%err%failed ) then
        call adv%handle_err( samr%levels(0)%boxes(1), step, tester )
        return
      end if
    end do
  end subroutine rhyme_muscl_hancock_advection_test


  subroutine rhyme_muscl_hancock_advection_factory_handle_err ( this, box, step, tester )
    implicit none

    class ( rhyme_muscl_hancock_advection_factory_t ), intent ( inout ) :: this
    type ( samr_box_t ), intent ( in ) :: box
    integer, intent ( in ) :: step
    type ( assertion_t ), intent ( inout ) :: tester

    type ( hydro_conserved_t ) :: state
    character ( len=128 ) :: hint

    write ( hint, '(I0,A,I0,A,I0,A,I0,A,I0)' ) step, ' (', this%err%i,', ', &
      this%err%j, ', ', this%err%k, ') : mh_adv_test_', this%err%axis

    if ( this%err%is_bg ) then
      state = this%bg
      hint = trim( hint ) // ' (bg)'
    else
      state = this%slab
      hint = trim( hint ) // ' (slab)'
    end if

    call tester%expect( box%hydro(this%err%i, this%err%j, this%err%j)%u &
      .toBe. state%u .hint. hint )
  end subroutine rhyme_muscl_hancock_advection_factory_handle_err


  subroutine rhyme_muscl_hancock_advection_factory_set_ic_x( this, samr, bc, logger )
    implicit none

    class ( rhyme_muscl_hancock_advection_factory_t ), intent ( inout ) :: this
    type ( samr_t ), intent ( inout ) :: samr
    type ( samr_bc_t ), intent ( inout ) :: bc
    type ( log_t ), intent ( inout ) :: logger

    integer :: i, j

    samr = samr_factory%generate_with( &
      this%nlevels, &
      [ this%ngrids, this%ngrids, 1 ], &
      [ 2, 2, 0 ], &
      this%max_nboxes, &
      this%init_nboxes, &
      [ this%ngrids * this%dx, this%ngrids * this%dx, 0.d0 ] &
    )

    do j = 1, samr%levels(0)%boxes(1)%dims(2)
      if ( j > this%slab_min_i .and. j <= this%slab_max_i ) then
        do i = 1, samr%levels(0)%boxes(1)%dims(1)
          samr%levels(0)%boxes(1)%hydro(i,j,1)%u = this%slab%u
        end do
      else
        do i = 1, samr%levels(0)%boxes(1)%dims(1)
          samr%levels(0)%boxes(1)%hydro(i,j,1)%u = this%bg%u
        end do
      end if
    end do

    bc%types = [ &
      bcid%periodic, bcid%periodic, & ! left, right
      bcid%outflow, bcid%outflow, & ! bottom, top
      bcid%outflow, bcid%outflow & ! back, front
    ]

    call bc%init( samr, logger )
  end subroutine rhyme_muscl_hancock_advection_factory_set_ic_x


  subroutine rhyme_muscl_hancock_advection_factory_set_ic_y ( this, samr, bc, logger )
    implicit none

    class ( rhyme_muscl_hancock_advection_factory_t ), intent ( inout ) :: this
    type ( samr_t ), intent ( inout ) :: samr
    type ( samr_bc_t ), intent ( inout ) :: bc
    type ( log_t ), intent ( inout ) :: logger

    integer :: i, j

    samr = samr_factory%generate_with( &
      this%nlevels, &
      [ this%ngrids, this%ngrids, 1 ], &
      [ 2, 2, 0 ], &
      this%max_nboxes, &
      this%init_nboxes, &
      [ this%ngrids * this%dx, this%ngrids * this%dx, 0.d0 ] &
    )

    do j = 1, samr%levels(0)%boxes(1)%dims(2)
        do i = 1, samr%levels(0)%boxes(1)%dims(1)
          if ( i > this%slab_min_i .and. i <= this%slab_max_i ) then
            samr%levels(0)%boxes(1)%hydro(i,j,1)%u = this%slab%u
          else
            samr%levels(0)%boxes(1)%hydro(i,j,1)%u = this%bg%u
          end if
        end do
    end do

    bc%types = [ &
      bcid%outflow, bcid%outflow, & ! left, right
      bcid%periodic, bcid%periodic, & ! bottom, top
      bcid%outflow, bcid%outflow & ! back, front
    ]

    call bc%init( samr, logger )
  end subroutine rhyme_muscl_hancock_advection_factory_set_ic_y


  subroutine rhyme_muscl_hancock_advection_factory_set_ic_z ( this, samr, bc, logger )
    implicit none

    class ( rhyme_muscl_hancock_advection_factory_t ), intent ( inout ) :: this
    type ( samr_t ), intent ( inout ) :: samr
    type ( samr_bc_t ), intent ( inout ) :: bc
    type ( log_t ), intent ( inout ) :: logger

    integer :: i, k

    samr = samr_factory%generate_with( &
      this%nlevels, &
      [ this%ngrids, 1, this%ngrids ], &
      [ 2, 0, 3 ], &
      this%max_nboxes, &
      this%init_nboxes, &
      [ this%ngrids * this%dx, 0.d0, this%ngrids * this%dx ] &
    )

    do k = 1, samr%levels(0)%boxes(1)%dims(3)
        do i = 1, samr%levels(0)%boxes(1)%dims(1)
          if ( i > this%slab_min_i .and. i <= this%slab_max_i ) then
            samr%levels(0)%boxes(1)%hydro(i,1,k)%u = this%slab%u
          else
            samr%levels(0)%boxes(1)%hydro(i,1,k)%u = this%bg%u
          end if
        end do
    end do

    bc%types = [ &
      bcid%outflow, bcid%outflow, & ! left, right
      bcid%outflow, bcid%outflow, & ! bottom, top
      bcid%periodic, bcid%periodic & ! back, front
    ]

    call bc%init( samr, logger )
  end subroutine rhyme_muscl_hancock_advection_factory_set_ic_z


  subroutine rhyme_muscl_hancock_advection_factory_test_x ( this, box )
    implicit none

    class ( rhyme_muscl_hancock_advection_factory_t ), intent ( inout ) :: this
    type ( samr_box_t ), intent ( in ) :: box

    integer :: i, j

    do j = 1, box%dims(2)
      if ( j > this%slab_min_i .and. j <= this%slab_max_i ) then
        do i = 1, box%dims(1)
          if ( any( ieee_is_nan( box%hydro(i, j, 1)%u ) ) &
          .or. any( abs( box%hydro(i, j, 1)%u - this%slab%u ) > epsilon(0.e0) ) ) then
            this%err%i = i; this%err%j = j; this%err%k = 1
            this%err%is_bg = .false.
            this%err%failed = .true.
            return
          end if
        end do
      else
        do i = 1, box%dims(1)
          if ( any( ieee_is_nan( box%hydro(i, j, 1)%u ) ) &
          .or. any( abs( box%hydro(i, j, 1)%u - this%bg%u ) > epsilon(0.e0) ) ) then
            this%err%i = i; this%err%j = j; this%err%k = 1
            this%err%is_bg = .true.
            this%err%failed = .true.
            return
          end if
        end do
      end if
    end do
  end subroutine rhyme_muscl_hancock_advection_factory_test_x


  subroutine rhyme_muscl_hancock_advection_factory_test_y ( this, box )
    implicit none

    class ( rhyme_muscl_hancock_advection_factory_t ), intent ( inout ) :: this
    type ( samr_box_t ), intent ( in ) :: box

    integer :: i, j

    do j = 1, box%dims(2)
      do i = 1, box%dims(1)
        if ( i > this%slab_min_i .and. i <= this%slab_max_i ) then
          if ( any( ieee_is_nan( box%hydro(i, j, 1)%u ) ) &
          .or. any( abs( box%hydro(i, j, 1)%u - this%slab%u ) > epsilon(0.e0) ) ) then
            this%err%i = i; this%err%j = j; this%err%k = 1
            this%err%is_bg = .false.
            this%err%failed = .true.
            return
          end if
        else
          if ( any( ieee_is_nan( box%hydro(i, j, 1)%u ) ) &
          .or. any( abs( box%hydro(i, j, 1)%u - this%bg%u ) > epsilon(0.e0) ) ) then
            this%err%i = i; this%err%j = j; this%err%k = 1
            this%err%is_bg = .true.
            this%err%failed = .true.
            return
          end if
        end if
      end do
    end do
  end subroutine rhyme_muscl_hancock_advection_factory_test_y


  subroutine rhyme_muscl_hancock_advection_factory_test_z ( this, box )
    implicit none

    class ( rhyme_muscl_hancock_advection_factory_t ), intent ( inout ) :: this
    type ( samr_box_t ), intent ( in ) :: box

    integer :: i, k

    do k = 1, box%dims(3)
      do i = 1, box%dims(1)
        if ( i > this%slab_min_i .and. i <= this%slab_max_i ) then
          if ( any( ieee_is_nan( box%hydro(i, 1, k)%u ) ) &
          .or. any( abs( box%hydro(i, 1, k)%u - this%slab%u ) > epsilon(0.e0) ) ) then
            this%err%i = i; this%err%j = 1; this%err%k = k
            this%err%is_bg = .false.
            this%err%failed = .true.
            return
          end if
        else
          if ( any( ieee_is_nan( box%hydro(i, 1, k)%u ) ) &
          .or. any( abs( box%hydro(i, 1, k)%u - this%bg%u ) > epsilon(0.e0) ) ) then
            this%err%i = i; this%err%j = 1; this%err%k = k
            this%err%is_bg = .true.
            this%err%failed = .true.
            return
          end if
        end if
      end do
    end do
  end subroutine rhyme_muscl_hancock_advection_factory_test_z
end module rhyme_muscl_hancock_advection_factory
