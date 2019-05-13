module rhyme_muscl_hancock_factory
  use rhyme_samr_factory
  use rhyme_muscl_hancock
  use rhyme_cfl
  use rhyme_ideal_gas_factory
  use rhyme_slope_limiter
  use rhyme_ideal_gas_factory

  implicit none

  logical :: mh_fac_initialized = .false.

  integer, parameter :: mh_fac_nlevels = 4
  integer, parameter :: mh_fac_base_grid(3) = [ 16, 8, 1 ]
  integer, parameter :: mh_fac_ghost_cells(3) = [ 2, 2, 0 ]
  integer, parameter :: mh_fac_max_nboxes ( 0:samrid%max_nlevels ) = [ &
    1, 3, 9, 27, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 &
  ]
  integer, parameter :: mh_fac_init_nboxes ( 0:samrid%max_nlevels ) = [ &
    1, 2, 4, 8, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 &
  ]
  integer, parameter :: mh_fac_max_nboxes_uni ( 0:samrid%max_nlevels ) = [ &
    1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 &
  ]
  integer, parameter :: mh_fac_init_nboxes_uni ( 0:samrid%max_nlevels ) = [ &
    1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 &
  ]

  real ( kind=8 ), parameter :: mh_fac_courant_number = 0.23
  integer, parameter :: mh_fac_gastype = igid%diatomic
  real ( kind=8 ), parameter :: mh_fac_gamma = 7.d0 / 5.d0
  integer, parameter :: mh_fac_sltype = slid%minmod
  integer, parameter :: mh_fac_n_iteration = 100
  real ( kind=8 ), parameter :: mh_fac_tolerance = 1.d-6
  real ( kind=8 ), parameter :: mh_fac_pressure_floor = 1.d-10

  type ( cfl_t ) :: mh_fac_cfl
  type ( chemistry_t ) :: mh_fac_chemi
  type ( thermo_base_t ) :: mh_fac_thermo
  type ( ideal_gas_t ) :: mh_fac_ig
  type ( irs_t ) :: mh_fac_irs
  type ( slope_limiter_t ) :: mh_fac_sl
  type ( samr_t ) :: mh_fac_samr
  type ( log_t ) :: mh_fac_log

contains

  subroutine rhyme_muscl_hancock_factory_init ()
    implicit none

    if ( mh_fac_initialized ) return

    ! Initializing SAMR
    mh_fac_samr = samr_factory%generate()

    ! Initializing CFL
    mh_fac_cfl%courant_number = mh_fac_courant_number

    ! Initializing Ideal Gas
    mh_fac_ig = ig_factory%generate( mh_fac_gastype )

    ! Initializing Iteratice Riemann Solver
    call mh_fac_irs%init( mh_fac_log )

    ! Initializing Slope Limiter
    mh_fac_sl%type = mh_fac_sltype

    ! Initializing Iterative Riemann Solver Configs
    mh_fac_irs%n_iteration = mh_fac_n_iteration
    mh_fac_irs%tolerance = mh_fac_tolerance
    mh_fac_irs%pressure_floor = mh_fac_pressure_floor

    mh_fac_initialized = .true.
  end subroutine rhyme_muscl_hancock_factory_init

end module rhyme_muscl_hancock_factory
