module rhyme_drawing_factory
  use rhyme_drawing
  use rhyme_samr_factory
  use rhyme_hydro_base_factory
  use rhyme_log

  logical :: rhyme_drawing_factory_initialized = .false.

  integer, parameter :: nlevels = 1
  integer, parameter :: base_grid(3) = [ 8, 10, 10 ]
  integer, parameter :: ghost_cells(3) = [ 0, 0, 0 ]
  integer, parameter :: max_nboxes( 0:samrid%max_nlevels ) = [ &
    1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 &
  ]
  integer, parameter :: init_nboxes( 0:samrid%max_nlevels ) = [ &
    1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 &
  ]
  integer, parameter :: xl(3) = [ 4, 6, 8 ]
  integer, parameter :: l(3) = [ 4, 2, 1 ]

  type ( rhyme_hydro_factory_t ) :: hy


  type ( chemistry_t ) :: chemi
  type ( thermo_base_t ) :: thermo
  type ( ideal_gas_t ) :: ig
  type ( log_t ) :: log
  type ( samr_t ) :: samr

contains

  subroutine rhyme_drawing_factory_init ()
    implicit none

    if ( rhyme_drawing_factory_initialized ) return

    call hy%init

    call chemi%init( log )
    call thermo%init( log )
    call ig%init_with( chemi, thermo, igid%diatomic, log )

    call rhyme_samr_factory_fill ( &
      nlevels, base_grid, ghost_cells, max_nboxes, init_nboxes, samr &
    )

    rhyme_drawing_factory_initialized = .true.
  end subroutine rhyme_drawing_factory_init
end module rhyme_drawing_factory
