module rhyme_muscl_hancock_advection_factory
  use rhyme_muscl_hancock_factory
  use rhyme_thermo_base_factory
  use rhyme_samr_bc_factory
  use rhyme_samr_factory
  use rhyme_cfl_factory

  implicit none

  real ( kind=8 ), parameter :: rho_state = 1.2d0
  real ( kind=8 ), parameter :: rho_bg = 1.d0
  real ( kind=8 ), parameter :: v = .01d0
  real ( kind=8 ), parameter :: p = .1d0
  real ( kind=8 ), parameter :: gamma = 7.d0 / 5.d0
  real ( kind=8 ), parameter :: cs = sqrt( gamma * p / rho_bg )
  integer, parameter :: gas_type = thid%diatomic


  type, private :: rhyme_muscl_hancock_advection_factory_t
    logical :: initialized = .false.
  contains
    procedure :: init => rhyme_muscl_hancock_advection_factory_init
    procedure :: generate => rhyme_muscl_hancock_advection_factory_generate
  end type rhyme_muscl_hancock_advection_factory_t

  type ( rhyme_muscl_hancock_advection_factory_t ) :: mh_adv_factory = &
    rhyme_muscl_hancock_advection_factory_t()


  type :: muscl_hancock_advection_test_t
    integer :: grid( NDIM ), ghosts( NDIM )
    integer :: nboxes( 0:samrid%max_nlevels )
    real ( kind=8 ) :: states( cid%rho:cid%e_tot, NDIM ), bg( cid%rho:cid%p, NDIM )
    real ( kind=8 ) :: dx( NDIM ), dt( NDIM ), box_length( NDIM )
    real ( kind=8 ) :: v( cid%u:cid%u+NDIM-1, NDIM )
    type ( thermo_base_t ) :: thermo
    type ( samr_t ) :: samr( NDIM )
    type ( samr_bc_t ) :: bc( NDIM )
    type ( cfl_t ) :: cfl( NDIM )
  contains
    procedure :: init => muscl_hancock_advection_test_init
  end type muscl_hancock_advection_test_t

contains

  subroutine rhyme_muscl_hancock_advection_factory_init ( this )
    implicit none

    class ( rhyme_muscl_hancock_advection_factory_t ), intent ( inout ) :: this

    this%initialized = .true.
  end subroutine rhyme_muscl_hancock_advection_factory_init


  subroutine muscl_hancock_advection_test_init ( this, grid )
    use rhyme_log_factory

    implicit none

    class ( muscl_hancock_advection_test_t ), intent ( inout ) :: this
    integer, intent ( in ) :: grid( NDIM )

    type ( log_t ) :: logger
    integer :: d

#if NDIM == 1
#define V_ARRAY [ 0.d0 ]
#elif NDIM == 2
#define V_ARRAY [ 0.d0, 0.d0 ]
#elif NDIM == 3
#define V_ARRAY [ 0.d0, 0.d0, 0.d0 ]
#endif

    this%grid = grid
    this%ghosts = merge( 2, 0, grid > 1 )
    this%nboxes = 0
    this%nboxes(0) = 1
    this%box_length = 1.d0 * real( grid / grid(1), kind=8 )

    this%dx = 1.d0 / this%grid

    do d = 1, NDIM
      this%v( cid%u:cid%u+NDIM-1, d ) = V_ARRAY
      this%v( cid%u+d-1, d ) = v
    end do

    this%dt = this%dx / v


    this%thermo = th_factory%generate( gas_type )
    logger = log_factory%generate()

    call rhyme_thermo_base_init( this%thermo, logger )


    do d = 1, NDIM
      this%cfl(d) = cfl_factory%generate( this%dt(d) * abs( v + cs ) / this%dx(d) )

      call conv_prim_vars_to_cons( rho_state, this%v( cid%u:cid%u+NDIM-1, d ), p, &
        this%states( cid%rho:cid%e_tot, d ) )
      call conv_prim_vars_to_cons( rho_bg, this%v( cid%u:cid%u+NDIM-1, d ), p, &
        this%bg( cid%rho:cid%e_tot, d ) )
    end do
  end subroutine muscl_hancock_advection_test_init


  function rhyme_muscl_hancock_advection_factory_generate ( this, grid ) result ( mh_adv )
    implicit none

    class ( rhyme_muscl_hancock_advection_factory_t ), intent ( inout ) :: this
    integer, intent ( in ) :: grid( NDIM )
    type ( muscl_hancock_advection_test_t ) :: mh_adv

#if NDIM == 1
#define LOOP_J
#define LOOP_J_END
#define LOOP_K
#define LOOP_K_END
#define JDX
#define KDX
#elif NDIM == 2
#define LOOP_J do j = 1, mh_adv%samr(d)%levels(0)%boxes(1)%dims(2)
#define LOOP_J_END end do
#define LOOP_K
#define LOOP_K_END
#define JDX ,j
#define KDX
#elif NDIM == 3
#define LOOP_J do j = 1, mh_adv%samr(d)%levels(0)%boxes(1)%dims(2)
#define LOOP_J_END end do
#define LOOP_K do k = 1, mh_adv%samr(d)%levels(0)%boxes(1)%dims(3)
#define LOOP_K_END end do
#define JDX ,j
#define KDX ,k
#endif

    integer :: i JDX KDX, d
    integer :: lambda( NDIM )

    call this%init
    call mh_adv%init( grid )

    do d = 1, NDIM
      mh_adv%bc(d)%types = bcid%outflow
      mh_adv%bc(d)%types( (d-1)*2+1:(d-1)*2+2 ) = bcid%periodic
      mh_adv%samr(d) = samr_factory%generate_with( 1, mh_adv%grid, &
        mh_adv%ghosts, mh_adv%nboxes, mh_adv%nboxes, mh_adv%box_length )

      lambda = mh_adv%samr(d)%levels(0)%boxes(1)%dims / 4

      LOOP_K
        LOOP_J
          do i = 1, mh_adv%samr(d)%levels(0)%boxes(1)%dims(1)
            select case ( d )
            case ( samrid%x )
              if ( mod( i, lambda(1) ) >= lambda(1) / 2 ) then
                mh_adv%samr(d)%levels(0)%boxes(1)%cells( &
                  i JDX KDX, cid%rho:cid%e_tot ) = mh_adv%states( cid%rho:cid%e_tot, d )
              else
                mh_adv%samr(d)%levels(0)%boxes(1)%cells( &
                  i JDX KDX, cid%rho:cid%e_tot ) = mh_adv%bg( cid%rho:cid%e_tot, d)
              end if
#if NDIM > 1
            case ( samrid%y )
              if ( mod( j, lambda(2) ) >= lambda(2) / 2 ) then
                mh_adv%samr(d)%levels(0)%boxes(1)%cells( &
                  i JDX KDX, cid%rho:cid%e_tot ) = mh_adv%states( cid%rho:cid%e_tot, d )
              else
                mh_adv%samr(d)%levels(0)%boxes(1)%cells( &
                  i JDX KDX, cid%rho:cid%e_tot ) = mh_adv%bg( cid%rho:cid%e_tot, d)
              end if
#endif
#if NDIM > 2
            case ( samrid%z )
              if ( mod( k, lambda(3) ) >= lambda(3) / 2 ) then
                mh_adv%samr(d)%levels(0)%boxes(1)%cells( &
                  i JDX KDX, cid%rho:cid%e_tot ) = mh_adv%states( cid%rho:cid%e_tot, d )
              else
                mh_adv%samr(d)%levels(0)%boxes(1)%cells( &
                  i JDX KDX, cid%rho:cid%e_tot ) = mh_adv%bg( cid%rho:cid%e_tot, d)
              end if
#endif
            end select
          end do
        LOOP_J_END
      LOOP_K_END

    end do
  end function rhyme_muscl_hancock_advection_factory_generate
end module rhyme_muscl_hancock_advection_factory
