module rhyme_samr_factory
  use rhyme_samr

  implicit none

  logical :: initialized = .false.
  integer :: tot_nboxes(0:samrid%max_nlevels) = 0
  integer, parameter :: xdim = 16
  integer, parameter :: ydim = 8
  integer, parameter :: zdim = 4
  integer, parameter :: nlevels = 3
  integer, parameter :: ghost_cells(3) = [2, 1, 0]
  real(kind=8), parameter :: x = 1.23d0
  real(kind=8), parameter :: y = 2.34d0
  real(kind=8), parameter :: z = 3.45d0
  type(samr_t) :: samr_fac


contains

  subroutine rhyme_samr_factory_init ()
    implicit none

    integer :: i, j, k

    if ( initialized ) return

    tot_nboxes(0:2) = [ 1, 10, 100 ]

    call samr_fac%init_with ( [xdim, ydim, zdim], nlevels, tot_nboxes, ghost_cells )

    do k = 1, zdim
      do j = 1, ydim
        do i = 1, xdim
          samr_fac%levels(0)%boxes(1)%hydro(i, j, k)%u = i * x + j * y + k * z
        end do
      end do
    end do

  end subroutine rhyme_samr_factory_init
end module rhyme_samr_factory
