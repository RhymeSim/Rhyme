module rhyme_muscl_hancock_factory
  use rhyme_samr_factory
  use rhyme_muscl_hancock
  use rhyme_cfl
  use rhyme_ideal_gas
  use rhyme_slope_limiter

  implicit none

  logical :: mh_fact_initialized = .false.

  integer, parameter :: nlevels = 4
  integer, parameter :: base_grid(3) = [ 16, 8, 1 ]
  integer, parameter :: ghost_cells(3) = [ 2, 2, 0 ]
  integer, parameter :: max_nboxes ( 0:samrid%max_nlevels ) = [ &
    1, 3, 9, 27, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 &
  ]
  integer, parameter :: init_nboxes ( 0:samrid%max_nlevels ) = [ &
    1, 2, 4, 8, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 &
  ]

  real ( kind=8 ), parameter :: courant_number = 0.23
  integer, parameter :: gastype = igid%monatomic
  integer, parameter :: sltype = slid%minmod
  integer, parameter :: n_iteration = 100
  real ( kind=8 ), parameter :: tolerance = 1.d-6
  real ( kind=8 ), parameter :: pressure_floor = 1.d-10

  type ( cfl_t ) :: cfl
  type ( ideal_gas_t ) :: ig
  type ( iterative_riemann_solver_t ) :: irs
  type ( slope_limiter_t ) :: sl
  type ( samr_t ) :: samr

contains

  subroutine rhyme_muscl_hancock_factory_init ()
    implicit none

    if ( mh_fact_initialized ) return


    ! Initializing SAMR
    call rhyme_samr_factory_fill( &
      nlevels, base_grid, ghost_cells, max_nboxes, init_nboxes, samr )

    ! Initializing CFL
    cfl%courant_number = courant_number

    ! Initializing Ideal Gas
    call ig%init_with ( gastype )

    ! Initializing Iteratice Riemann Solver
    call irs%init ( ig )

    ! Initializing Slope Limiter
    sl%type = sltype

    ! Initializing Iterative Riemann Solver Configs
    irs%n_iteration = n_iteration
    irs%tolerance = tolerance
    irs%pressure_floor = pressure_floor

    mh_fact_initialized = .true.
  end subroutine rhyme_muscl_hancock_factory_init
end module rhyme_muscl_hancock_factory
