module rhyme_initial_condition_factory
  use rhyme_initial_condition

  implicit none

  logical :: rhyme_initial_condition_factory_intialized = .false.

  integer, parameter :: nlevels = 4
  integer, parameter :: base_grid(3) = [ 16, 8, 4 ]
  integer, parameter :: ghost_cells(3) = [ 2, 1, 0 ]
  integer, parameter :: max_nboxes ( 0:samrid%max_nlevels ) = [ &
    1, 3, 9, 27, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 &
  ]
  type ( samr_t ) :: samr = samr_t ( &
    .false.,nlevels, base_grid, ghost_cells, max_nboxes &
  )

  integer, parameter :: nlevels_1d = 4
  integer, parameter :: base_grid_1d(3) = [ 32, 1, 1 ]
  integer, parameter :: ghost_cells_1d(3) = [ 2, 0, 0 ]
  integer, parameter :: max_nboxes_uni ( 0:samrid%max_nlevels ) = [ &
    1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 &
  ]
  type ( samr_t ) :: samr1d = samr_t ( &
    .false., nlevels_1d, base_grid_1d, ghost_cells_1d, max_nboxes &
  )

  integer, parameter :: nlevels_uni = 1
  integer, parameter :: base_grid_uni(3) = [ 32, 1, 1 ]
  integer, parameter :: ghost_cells_uni(3) = [ 2, 0, 0 ]
  integer, parameter :: nboxes ( 0:samrid%max_nlevels ) = [ &
    1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 &
  ]
  type ( samr_t ) :: samr_uni = samr_t ( &
    .false., nlevels_uni, base_grid_uni, ghost_cells_uni, max_nboxes_uni &
  )

end module rhyme_initial_condition_factory
