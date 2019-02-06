module rhyme_mh_workspace_factory
  use rhyme_mh_workspace
  use rhyme_samr

  implicit none

  logical :: initialized = .false.

  integer, parameter :: base_grid(3) = [ 16, 8 , 1 ]
  integer, parameter :: nlevels = 3
  integer, parameter :: ghost_cells(3) = [ 2, 1, 0 ]

  integer :: tot_nboxes ( 0:23 )
  integer :: nboxes ( 0:nlevels )

  type(samr_t) :: samr

contains

  subroutine rhyme_mh_workspace_factory_init ()
    implicit none

    integer :: l, b, i, j, k, uid, lb(3), ub(3), dims(3)
    integer :: length
    real ( kind=8 ) :: val

    if ( initialized ) return

    tot_nboxes(0:3) = [ 1, 3, 9 , 27 ]
    nboxes = [ 1, 2, 4, 8 ]

    call samr%init_with ( base_grid, nlevels, tot_nboxes, ghost_cells )

    do l = 1, samr%nlevels
      length = nboxes(l)
      samr%levels(l)%nboxes = length
      allocate ( samr%levels(l)%boxes(length) )

      do b = 1, nboxes(l)
        lb = - ghost_cells
        dims = base_grid + l * 10 + b * 1
        ub = dims + ghost_cells

        samr%levels(l)%boxes(b)%dims(:) = dims(:)
        ! allocate ( samr%levels(l)%boxes(b)%hydro(lb(1):ub(1), lb(2):ub(2), lb(3):ub(3)) )
      end do
    end do

    ! do l = 0, samr%nlevels
    !   do b = 0, samr%levels(l)%nboxes
    !     do k = 1, samr%levels(l)%boxes(b)%dims(3)
    !       do j = 1, samr%levels(l)%boxes(b)%dims(2)
    !         do i = 1, samr%levels(l)%boxes(b)%dims(1)
    !           val = l * 1d1 + b * 1d0 + i * 1d-1 + j * 1d-2 + k * 1d-3
    !           do uid = hyid%rho, hyid%e_tot
    !             samr%levels(l)%boxes(b)%hydro(i,j,k)%u = val + uid * 1d-4
    !           end do
    !         end do
    !       end do
    !     end do
    !   end do
    ! end do

    initialized = .true.
  end subroutine rhyme_mh_workspace_factory_init
end module rhyme_mh_workspace_factory
