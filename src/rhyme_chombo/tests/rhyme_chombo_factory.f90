module rhyme_chombo_factory
  use rhyme_samr_factory
  use rhyme_chombo

  implicit none

  integer, parameter :: chombo_fac_base_grid(3) = [ 16, 8, 1 ]
  integer, parameter :: chombo_fac_nlevels = 3
  integer, parameter :: chombo_fac_ghost_cells(3) = [ 2, 2, 0 ]
  integer, parameter :: chombo_fac_max_nboxes ( 0:samrid%max_nlevels ) = [ &
    1, 10, 100, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 &
  ]
  integer, parameter :: chombo_fac_init_nboxes ( 0:samrid%max_nlevels ) = [ &
    1, 2, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 &
  ]

  type(samr_t) :: chombo_fac_samr

contains

  subroutine rhyme_chombo_factory_init ()
    implicit none

    call rhyme_samr_factory_fill( &
      chombo_fac_nlevels, &
      chombo_fac_base_grid, &
      chombo_fac_ghost_cells, &
      chombo_fac_max_nboxes, &
      chombo_fac_init_nboxes, &
      chombo_fac_samr &
    )

  end subroutine rhyme_chombo_factory_init
end module rhyme_chombo_factory
