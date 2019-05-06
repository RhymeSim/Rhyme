module rhyme_drawing_factory
  use rhyme_drawing
  use rhyme_ideal_gas_factory
  use rhyme_samr_factory
  use rhyme_hydro_base_factory
  use rhyme_log

  implicit none

  logical :: rhyme_drawing_factory_initialized = .false.

  type ( rhyme_hydro_factory_t ) :: hy

  type ( ideal_gas_t ) :: draw_fac_ig_di
  type ( ideal_gas_t ) :: draw_fac_ig_mon
  type ( log_t ) :: draw_fac_log
  type ( samr_t ) :: draw_fac_samr

contains

  subroutine rhyme_drawing_factory_init ()
    implicit none

    integer, parameter :: nlevels = 1
    integer, parameter :: base_grid(3) = [ 8, 10, 10 ]
    integer, parameter :: ghost_cells(3) = [ 0, 0, 0 ]
    integer, parameter :: max_nboxes( 0:samrid%max_nlevels ) = [ &
      1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 &
    ]
    integer, parameter :: init_nboxes( 0:samrid%max_nlevels ) = [ &
      1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 &
    ]

    if ( rhyme_drawing_factory_initialized ) return

    call hy%init
    call rhyme_ideal_gas_factory_init

    draw_fac_ig_mon%type = igid%monatomic
    call rhyme_ideal_gas_init( draw_fac_ig_mon, ig_chemi, ig_thermo, ig_units, draw_fac_log )
    draw_fac_ig_di%type = igid%monatomic
    call rhyme_ideal_gas_init( draw_fac_ig_di, ig_chemi, ig_thermo, ig_units, draw_fac_log )

    call rhyme_samr_factory_fill ( &
      nlevels, base_grid, ghost_cells, max_nboxes, init_nboxes, draw_fac_samr )

    rhyme_drawing_factory_initialized = .true.
  end subroutine rhyme_drawing_factory_init
end module rhyme_drawing_factory
