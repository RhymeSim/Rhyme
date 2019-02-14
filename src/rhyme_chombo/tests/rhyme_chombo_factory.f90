module rhyme_chombo_factory
  use rhyme_chombo
  use rhyme_samr


  implicit none


  integer, parameter :: base_grid(3) = [ 16, 8, 1 ]
  integer, parameter :: nlevels = 3
  integer, parameter :: ghost_cells(3) = [ 2, 1, 1 ]

  integer, parameter :: max_nboxes ( 0:samrid%max_nlevels ) = [ &
    1, 10, 100, 1000, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 &
  ]
  type(samr_t) :: samr

contains

  subroutine rhyme_chombo_factory_init ()
    implicit none

    call samr%init_with ( base_grid, nlevels, max_nboxes, ghost_cells )
  end subroutine rhyme_chombo_factory_init

end module rhyme_chombo_factory
