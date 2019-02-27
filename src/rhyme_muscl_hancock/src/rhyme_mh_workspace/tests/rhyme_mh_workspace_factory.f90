module rhyme_mh_workspace_factory
  use rhyme_samr_factory
  use rhyme_mh_workspace

  implicit none

  integer, parameter :: nlevels = 4
  integer, parameter :: base_grid(3) = [ 16, 8 , 1 ]
  integer, parameter :: ghost_cells(3) = [ 2, 1, 0 ]
  integer, parameter :: max_nboxes ( 0:samrid%max_nlevels ) = [ &
    1, 3, 9, 27, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 &
  ]
  integer, parameter :: init_nboxes ( 0:samrid%max_nlevels ) = [ &
    1, 2, 4, 8, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 &
  ]

  type(samr_t) :: samr

contains

  subroutine rhyme_mh_workspace_factory_init ()
    implicit none

    call rhyme_samr_factory_fill( &
      nlevels, base_grid, ghost_cells, max_nboxes, init_nboxes, samr )

  end subroutine rhyme_mh_workspace_factory_init
end module rhyme_mh_workspace_factory
