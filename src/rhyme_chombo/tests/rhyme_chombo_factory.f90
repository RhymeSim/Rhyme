module rhyme_chombo_factory
  use rhyme_chombo
  use rhyme_samr


  implicit none


  integer, parameter :: xdim = 16
  integer, parameter :: ydim = 8
  integer, parameter :: zdim = 1
  integer, parameter :: nlevels = 3
  integer, parameter :: ghost_cells(3) = [ 2, 1, 1 ]

  integer :: tot_nboxes(0:23) = 0
  type(samr_t) :: samr

contains

  subroutine rhyme_chombo_factory_init ()
    implicit none

    tot_nboxes(0:3) = [ 1, 10, 100, 1000 ]

    call samr%init_with ( [ xdim, ydim, zdim ], nlevels, tot_nboxes, ghost_cells )
  end subroutine rhyme_chombo_factory_init

end module rhyme_chombo_factory
