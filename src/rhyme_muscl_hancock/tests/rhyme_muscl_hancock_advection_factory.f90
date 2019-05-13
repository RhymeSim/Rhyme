module rhyme_muscl_hancock_advection_factory
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
    type ( hydro_primitive_t ) :: slab_base, bg_base
    type ( rhyme_muscl_hancock_advection_factory_err_t ) :: err
    logical :: initialized = .false.
  contains
    procedure :: init => rhyme_muscl_hancock_advection_factory_init
    procedure :: slab => rhyme_muscl_hancock_advection_factory_slab
    procedure :: bg => rhyme_muscl_hancock_advection_factory_bg
    procedure :: set_ic_x => rhyme_muscl_hancock_advection_factory_set_ic_x
    procedure :: test_x => rhyme_muscl_hancock_advection_factory_test_x
    procedure :: set_ic_y => rhyme_muscl_hancock_advection_factory_set_ic_y
    procedure :: test_y => rhyme_muscl_hancock_advection_factory_test_y
    procedure :: set_ic_z => rhyme_muscl_hancock_advection_factory_set_ic_z
    procedure :: test_z => rhyme_muscl_hancock_advection_factory_test_z
    procedure :: handle_err => rhyme_muscl_hancock_advection_factory_handle_err
  end type rhyme_muscl_hancock_advection_factory_t

contains

  subroutine rhyme_muscl_hancock_advection_factory_init ( this, cfl, ig )
    implicit none

    class ( rhyme_muscl_hancock_advection_factory_t ), intent ( inout ) :: this
    type ( cfl_t ), intent ( in ) :: cfl
    type ( ideal_gas_t ), intent ( in ) :: ig

    type ( hydro_conserved_t ) :: slab

    this%slab_base%w = [ 1.d0, 0.d0, 0.d0, 0.d0, .1d0 ]
    this%bg_base%w = [ 1.d0, 0.d0, 0.d0, 0.d0, .1d0 ]

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
    slab = this%slab( ig, hyid%x )
    this%dt = this%courant_number * this%dx / ig%p( slab )
    this%v = this%dx / this%dt

    this%initialized = .true.
  end subroutine rhyme_muscl_hancock_advection_factory_init


  function rhyme_muscl_hancock_advection_factory_slab ( this, ig, axis ) result ( slab )
    implicit none

    class ( rhyme_muscl_hancock_advection_factory_t ), intent ( inout ) :: this
    type ( ideal_gas_t ), intent ( in ) :: ig
    integer, intent ( in ) :: axis
    type ( hydro_conserved_t ) :: slab

    type ( hydro_primitive_t ) :: slab_prim
    integer :: idx

    slab_prim%w = this%slab_base%w

    select case ( axis )
    case ( hyid%x ); idx = hyid%u
    case ( hyid%y ); idx = hyid%v
    case ( hyid%z ); idx = hyid%w
    end select

    slab_prim%w( idx ) = this%v
    call ig%prim_to_cons( slab_prim, slab )
  end function rhyme_muscl_hancock_advection_factory_slab


  function rhyme_muscl_hancock_advection_factory_bg ( this, ig ) result ( bg )
    implicit none

    class ( rhyme_muscl_hancock_advection_factory_t ), intent ( inout ) :: this
    type ( ideal_gas_t ), intent ( in ) :: ig
    type ( hydro_conserved_t ) :: bg

    call ig%prim_to_cons( this%bg_base, bg )
  end function rhyme_muscl_hancock_advection_factory_bg


  subroutine rhyme_muscl_hancock_advection_test ( solver, ws, dir, tester )
    use rhyme_cfl_factory
    use rhyme_ideal_gas_factory
    use rhyme_samr_factory
    use rhyme_samr_bc_factory
    use rhyme_irs_factory
    use rhyme_slope_limiter_factory
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
    integer, intent ( in ) :: dir
    type ( assertion_t ), intent ( inout ) :: tester

    type ( muscl_hancock_t ) :: mh
    type ( cfl_t ) :: cfl
    type ( ideal_gas_t ) :: ig
    type ( samr_t ) :: samr
    type ( samr_bc_t ) :: bc
    type ( irs_t ) :: irs
    type ( slope_limiter_t ) :: sl
    type ( log_t ) :: logger

    type ( rhyme_muscl_hancock_advection_factory_t ) :: adv
    integer :: step

    cfl = cfl_factory%generate()
    ig = ig_factory%generate()
    samr = samr_factory%generate( physical=.true. )
    bc = bc_factory%generate()
    irs = irs_factory%generate()
    sl = sl_factory%generate()
    logger = log_factory%generate()

    call adv%init( cfl, ig )


    ! Setting up samr
    select case ( dir )
    case ( hyid%x )
      call adv%set_ic_x( ig, samr, bc, logger )
    case ( hyid%y )
      call adv%set_ic_y( ig, samr, bc, logger )
    case ( hyid%z )
      call adv%set_ic_z( ig, samr, bc, logger )
    end select

    ! Initializing MH and WS object
    call rhyme_muscl_hancock_init( mh, samr, ws, logger )

    do step = 1, 119 * adv%ngrids
      call bc%set_base_grid_boundaries( samr )

      call solver( mh, samr%levels(0)%boxes(1), &
        samr%box_lengths, &
        adv%dt, cfl, ig, irs, sl, ws )

      ! Test
      select case ( dir )
      case ( hyid%x )
        call adv%test_x( samr%levels(0)%boxes(1), ig )
        if ( adv%err%failed ) adv%err%axis = hyid%x
      case ( hyid%y )
        call adv%test_x( samr%levels(0)%boxes(1), ig )
        if ( adv%err%failed ) adv%err%axis = hyid%y
      case ( hyid%z )
        call adv%test_z( samr%levels(0)%boxes(1), ig )
        if ( adv%err%failed ) adv%err%axis = hyid%z
      end select

      samr%levels(0)%iteration = samr%levels(0)%iteration + 1

      if ( adv%err%failed ) then
        call adv%handle_err( samr%levels(0)%boxes(1), step, ig, tester )
        return
      end if
    end do
  end subroutine rhyme_muscl_hancock_advection_test


  subroutine rhyme_muscl_hancock_advection_factory_handle_err ( this, box, step, ig, tester )
    implicit none

    class ( rhyme_muscl_hancock_advection_factory_t ), intent ( inout ) :: this
    type ( samr_box_t ), intent ( in ) :: box
    integer, intent ( in ) :: step
    type ( ideal_gas_t ), intent ( in ) :: ig
    type ( assertion_t ), intent ( inout ) :: tester

    type ( hydro_conserved_t ) :: U, bg, slab
    character ( len=128 ) :: hint

    slab = this%slab( ig, this%err%axis )
    bg = this%bg( ig )
    write ( hint, '(I0,A,I0)' ) step, ': mh_adv_test_', this%err%axis

    if ( this%err%is_bg ) then
      U = bg
      hint = trim( hint ) // ' (bg)'
    else
      U = slab
      hint = trim( hint ) // ' (slab)'
    end if

    call tester%expect( box%hydro(this%err%i, this%err%j, this%err%j)%u .toBe. U%u .hint. hint )
  end subroutine rhyme_muscl_hancock_advection_factory_handle_err


  subroutine rhyme_muscl_hancock_advection_factory_set_ic_x( this, ig, samr, bc, logger )
    implicit none

    class ( rhyme_muscl_hancock_advection_factory_t ), intent ( inout ) :: this
    type ( ideal_gas_t ), intent ( in ) :: ig
    type ( samr_t ), intent ( inout ) :: samr
    type ( samr_bc_t ), intent ( inout ) :: bc
    type ( log_t ), intent ( inout ) :: logger

    integer :: i, j
    type ( hydro_conserved_t ) :: slab, bg

    slab = this%slab( ig, hyid%x )
    bg = this%bg( ig )

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
          samr%levels(0)%boxes(1)%hydro(i,j,1)%u = slab%u
        end do
      else
        do i = 1, samr%levels(0)%boxes(1)%dims(1)
          samr%levels(0)%boxes(1)%hydro(i,j,1)%u = bg%u
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


  subroutine rhyme_muscl_hancock_advection_factory_set_ic_y ( this, ig, samr, bc, logger )
    implicit none

    class ( rhyme_muscl_hancock_advection_factory_t ), intent ( inout ) :: this
    type ( ideal_gas_t ), intent ( in ) :: ig
    type ( samr_t ), intent ( inout ) :: samr
    type ( samr_bc_t ), intent ( inout ) :: bc
    type ( log_t ), intent ( inout ) :: logger

    integer :: i, j
    type ( hydro_conserved_t ) :: slab, bg

    slab = this%slab( ig, hyid%y )
    bg = this%bg( ig )

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
            samr%levels(0)%boxes(1)%hydro(i,j,1)%u = slab%u
          else
            samr%levels(0)%boxes(1)%hydro(i,j,1)%u = bg%u
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


  subroutine rhyme_muscl_hancock_advection_factory_set_ic_z ( this, ig, samr, bc, logger )
    implicit none

    class ( rhyme_muscl_hancock_advection_factory_t ), intent ( inout ) :: this
    type ( ideal_gas_t ), intent ( in ) :: ig
    type ( samr_t ), intent ( inout ) :: samr
    type ( samr_bc_t ), intent ( inout ) :: bc
    type ( log_t ), intent ( inout ) :: logger

    integer :: i, k
    type ( hydro_conserved_t ) :: slab, bg

    slab = this%slab( ig, hyid%z )
    bg = this%bg( ig )

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
            samr%levels(0)%boxes(1)%hydro(i,1,k)%u = slab%u
          else
            samr%levels(0)%boxes(1)%hydro(i,1,k)%u = bg%u
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


  subroutine rhyme_muscl_hancock_advection_factory_test_x ( this, box, ig )
    implicit none

    class ( rhyme_muscl_hancock_advection_factory_t ), intent ( inout ) :: this
    type ( samr_box_t ), intent ( in ) :: box
    type ( ideal_gas_t ), intent ( in ) :: ig

    type ( hydro_conserved_t ) :: slab, bg
    integer :: i, j

    slab = this%slab( ig, hyid%x )
    bg = this%bg( ig )

    do j = 1, box%dims(2)
      if ( j > this%slab_min_i .and. j <= this%slab_max_i ) then
        do i = 1, box%dims(1)
          if ( any( abs( box%hydro(i, j, 1)%u - slab%u ) > epsilon(0.e0) ) ) then
            this%err%i = i; this%err%j = j; this%err%k = 1
            this%err%is_bg = .false.
            this%err%failed = .true.
            return
          end if
        end do
      else
        do i = 1, box%dims(1)
          if ( any( abs( box%hydro(i, j, 1)%u - bg%u ) > epsilon(0.e0) ) ) then
            this%err%i = i; this%err%j = j; this%err%k = 1
            this%err%is_bg = .true.
            this%err%failed = .true.
            return
          end if
        end do
      end if
    end do
  end subroutine rhyme_muscl_hancock_advection_factory_test_x


  subroutine rhyme_muscl_hancock_advection_factory_test_y ( this, box, ig )
    implicit none

    class ( rhyme_muscl_hancock_advection_factory_t ), intent ( inout ) :: this
    type ( samr_box_t ), intent ( in ) :: box
    type ( ideal_gas_t ), intent ( in ) :: ig

    type ( hydro_conserved_t ) :: slab, bg
    integer :: i, j

    slab = this%slab( ig, hyid%y )
    bg = this%bg( ig )

    do j = 1, box%dims(2)
      do i = 1, box%dims(1)
        if ( i > this%slab_min_i .and. i <= this%slab_max_i ) then
          if ( any( abs( box%hydro(i, j, 1)%u - slab%u ) > epsilon(0.e0) ) ) then
            this%err%i = i; this%err%j = j; this%err%k = 1
            this%err%is_bg = .false.
            this%err%failed = .true.
            return
          end if
        else
          if ( any( abs( box%hydro(i, j, 1)%u - bg%u ) > epsilon(0.e0) ) ) then
            this%err%i = i; this%err%j = j; this%err%k = 1
            this%err%is_bg = .true.
            this%err%failed = .true.
            return
          end if
        end if
      end do
    end do
  end subroutine rhyme_muscl_hancock_advection_factory_test_y


  subroutine rhyme_muscl_hancock_advection_factory_test_z ( this, box, ig )
    implicit none

    class ( rhyme_muscl_hancock_advection_factory_t ), intent ( inout ) :: this
    type ( samr_box_t ), intent ( in ) :: box
    type ( ideal_gas_t ), intent ( in ) :: ig

    type ( hydro_conserved_t ) :: slab, bg
    integer :: i, k

    slab = this%slab( ig, hyid%z )
    bg = this%bg( ig )

    do k = 1, box%dims(3)
      do i = 1, box%dims(1)
        if ( i > this%slab_min_i .and. i <= this%slab_max_i ) then
          if ( any( abs( box%hydro(i, 1, k)%u - slab%u ) > epsilon(0.e0) ) ) then
            this%err%i = i; this%err%j = 1; this%err%k = k
            this%err%is_bg = .false.
            this%err%failed = .true.
            return
          end if
        else
          if ( any( abs( box%hydro(i, 1, k)%u - bg%u ) > epsilon(0.e0) ) ) then
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
