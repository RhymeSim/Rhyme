module rhyme_samr_bc_factory
  use rhyme_samr_factory
  use rhyme_samr_bc

  implicit none

  logical :: samr_bc_fac_initialized = .false.

  integer, parameter :: base_grid(3) = [ 16, 8, 4 ]
  integer, parameter :: nlevels = 3
  integer, parameter :: nboxes = 11
  integer, parameter :: ghost_cells(3) = [ 2, 2, 2 ]
  integer, parameter :: max_nboxes( 0:samrid%max_nlevels ) = [ &
    1, 10, 100, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 &
  ]
  integer, parameter :: init_nboxes( 0:samrid%max_nlevels ) = [ &
    1, 2, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 &
  ]

  integer :: bc_types(6) = [ &
    bcid%reflective, &
    bcid%outflow, &
    bcid%periodic, &
    bcid%reflective, &
    bcid%outflow, &
    bcid%periodic ]

  type ( samr_t ) :: samr
  type ( log_t ) :: log

contains

  subroutine rhyme_samr_bc_factory_init ()
    implicit none

    if ( samr_bc_fac_initialized ) return

    call rhyme_samr_factory_fill ( &
      nlevels, base_grid, ghost_cells, max_nboxes, init_nboxes, samr &
    )

    samr_bc_fac_initialized = .true.
  end subroutine rhyme_samr_bc_factory_init
end module rhyme_samr_bc_factory
