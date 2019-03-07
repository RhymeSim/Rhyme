logical function rhyme_cfl_dt_test () result (failed)
  use rhyme_samr_factory
  use rhyme_cfl
  use rhyme_log

  implicit none

  integer, parameter :: base_grid(3) = [ 4, 8, 1 ]
  integer, parameter :: nlevels = 1
  integer, parameter :: ghost_cells(3) = [ 2, 2, 0 ]
  integer, parameter :: max_nboxes( 0:samrid%max_nlevels ) = [ &
    1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 &
  ]
  integer, parameter :: init_nboxes( 0:samrid%max_nlevels ) = [ &
    1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 &
  ]

  type ( cfl_t ) :: cfl
  type ( chemistry_t ) :: chemi
  type ( thermo_base_t ) :: thermo
  type ( ideal_gas_t ) :: ig
  type ( samr_t ) :: samr
  type ( log_t ) :: log

  real(kind=8) :: dt, dt_expected

  call chemi%init( log )
  call thermo%init( log )
  call ig%init_with ( chemi, thermo, igid%diatomic, log )

  call rhyme_samr_factory_fill( &
    nlevels, base_grid, ghost_cells, max_nboxes, init_nboxes, samr )

  dt = cfl%dt ( ig, samr )
  dt_expected = cfl%courant_number * minval ( samr%levels(0)%dx ) / ig%cs ( samr%levels(0)%boxes(1)%hydro(1,8,1) )

  failed = abs ( dt - dt_expected ) > epsilon(0.d0)
end function rhyme_cfl_dt_test
