module rhyme_mh_workspace_factory
  use rhyme_mh_workspace
  use rhyme_samr

  implicit none

  logical :: initialized = .false.
  integer, parameter :: xdim = 16
  integer, parameter :: ydim = 8
  integer, parameter :: zdim = 1
  integer, parameter :: nlevels = 3
  integer, parameter :: nboxes = 11
  integer, parameter :: ghost_cells(3) = [2, 1, 0]
  real(kind=8), parameter :: x = 1.23d0
  real(kind=8), parameter :: y = 2.34d0
  real(kind=8), parameter :: z = 3.45d0

  integer :: tot_nboxes(0:23)
  type(samr_t) :: samr

contains

  subroutine rhyme_mh_workspace_factory_init ()
    implicit none

    integer :: i, j, k

    if ( initialized ) return

    tot_nboxes(0) = 1
    tot_nboxes(1) = 10
    tot_nboxes(2) = 100
    tot_nboxes(3) = 1000

    call samr%init_with ( [xdim, ydim, zdim], nlevels, tot_nboxes, ghost_cells )

    do k = 1, zdim
      do j = 1, ydim
        do i = 1, xdim
          samr%levels(0)%boxes(1)%hydro(i,j,k)%u = i * x + j * y + k * z
        end do
      end do
    end do

    initialized = .true.
  end subroutine rhyme_mh_workspace_factory_init
end module rhyme_mh_workspace_factory
