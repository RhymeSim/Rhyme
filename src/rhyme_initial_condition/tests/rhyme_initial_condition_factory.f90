module rhyme_initial_condition_factory
  use rhyme_initial_condition
  use rhyme_ideal_gas_factory

  implicit none

  logical :: ic_fac_intialized = .false.

  integer, parameter :: nlevels = 4
  integer, parameter :: base_grid(3) = [ 16, 8, 4 ]
  integer, parameter :: ghost_cells(3) = [ 2, 2, 2 ]
  integer, parameter :: max_nboxes ( 0:samrid%max_nlevels ) = [ &
    1, 3, 9, 27, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 &
  ]
  integer, parameter :: init_nboxes ( 0:samrid%max_nlevels ) = [ &
    1, 2, 4, 8, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 &
  ]
  type ( initial_condition_t ) :: simple = initial_condition_t ( &
    icid%simple, icid%unset, nlevels, base_grid, max_nboxes, '' &
  )

  integer, parameter :: nlevels_1d = 4
  integer, parameter :: base_grid_1d(3) = [ 32, 1, 1 ]
  integer, parameter :: ghost_cells_1d(3) = [ 2, 0, 0 ]
  integer, parameter :: max_nboxes_1d ( 0:samrid%max_nlevels ) = [ &
    1, 3, 9, 27, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 &
  ]
  type ( initial_condition_t ) :: simple1d = initial_condition_t ( &
    icid%simple, icid%unset, nlevels_1d, base_grid_1d, max_nboxes_1d, '' &
  )

  integer, parameter :: nlevels_uni = 1
  integer, parameter :: base_grid_uni(3) = [ 32, 1, 1 ]
  integer, parameter :: ghost_cells_uni(3) = [ 2, 0, 0 ]
  integer, parameter :: max_nboxes_uni ( 0:samrid%max_nlevels ) = [ &
    1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 &
  ]
  type ( initial_condition_t ) :: simple_uni = initial_condition_t ( &
    icid%simple, icid%unset, nlevels_uni, base_grid_uni, max_nboxes_uni, '' &
  )

  type ( log_t ) :: log
  type ( ideal_gas_t ) :: ig

contains

  subroutine rhyme_initial_condition_factory_init ()
    implicit none

    if ( ic_fac_intialized ) return

    call rhyme_ideal_gas_factory_init

    ig%type = igid%monatomic
    call rhyme_ideal_gas_init( ig, ig_chemi, ig_thermo, ig_units, log )

    ic_fac_intialized = .true.
  end subroutine rhyme_initial_condition_factory_init
end module rhyme_initial_condition_factory
