module rhyme_samr_bc_factory
  use rhyme_samr_factory
  use rhyme_samr_bc

  implicit none

  logical :: samr_bc_fac_initialized = .false.

  integer, parameter :: samr_bc_fac_base_grid(3) = [ 16, 8, 4 ]
  integer, parameter :: samr_bc_fac_nlevels = 3
  integer, parameter :: samr_bc_fac_nboxes = 11
  integer, parameter :: samr_bc_fac_ghost_cells(3) = [ 2, 2, 2 ]
  integer, parameter :: samr_bc_fac_max_nboxes( 0:samrid%max_nlevels ) = [ &
    1, 10, 100, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 &
  ]
  integer, parameter :: samr_bc_fac_init_nboxes( 0:samrid%max_nlevels ) = [ &
    1, 2, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 &
  ]

  integer :: samr_bc_fac_bc_types(6) = [ &
    bcid%reflective, &
    bcid%outflow, &
    bcid%periodic, &
    bcid%reflective, &
    bcid%outflow, &
    bcid%periodic ]

  type ( samr_t ) :: samr_bc_fac_samr
  type ( log_t ) :: samr_bc_fac_log

contains

  subroutine rhyme_samr_bc_factory_init ()
    implicit none

    if ( samr_bc_fac_initialized ) return

    call rhyme_samr_factory_fill ( &
      samr_bc_fac_nlevels, &
      samr_bc_fac_base_grid, &
      samr_bc_fac_ghost_cells, &
      samr_bc_fac_max_nboxes, &
      samr_bc_fac_init_nboxes, &
      samr_bc_fac_samr &
    )

    samr_bc_fac_initialized = .true.
  end subroutine rhyme_samr_bc_factory_init
end module rhyme_samr_bc_factory
