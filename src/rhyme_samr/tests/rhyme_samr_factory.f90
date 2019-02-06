module rhyme_samr_factory
  use rhyme_samr

  implicit none

  integer, parameter :: nlevels = 3
  integer, parameter :: base_grid(3) = [ 16, 8, 4 ]
  integer, parameter :: ghost_cells(3) = [ 2, 1, 0 ]

  integer, parameter :: base_grid_1d(3) = [ 32, 1, 1 ]
  integer, parameter :: ghost_cells_1d(3) = [ 2, 0, 0 ]

  integer, parameter :: max_nboxes ( 0:samrid%max_nlevels ) = [
    1, 3, 9, 27, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
  ]
  integer, parameter :: nboxes ( 0:samrid%max_nlevels ) = [
    1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
  ]

  type(samr_t) :: samr, samr1d, samr_uniform


contains

  subroutine rhyme_samr_factory_init ()
    implicit none

    integer :: l, b, i, j, k, uid, lb(3), ub(3), lb1d(3), ub1d(3)
    real ( kind=8 ) :: val


    lb = - ghost_cells
    ub = base_grid + ghost_cells

    ! Initializing samr
    samr%nlevels = nlevels
    samr%max_nboxes(:) = max_nboxes(:)
    samr%ghost_cells(:) = ghost_cells(:)
    samr%base_grid(:) = base_grid(:)

    allocate ( samr%levels(0)%boxes(1)%hydro (
      lb(1):ub(1), lb(2):ub(2), lb(3):ub(3)
    ))

    do l = 0, samr%nlevels - 1
      do b = 1, samr%levels(l)%nboxes
        do k = 1, samr%levels(l)%boxes(b)%dims(3)
          do j = 1, samr%levels(l)%boxes(b)%dims(2)
            do i = 1, samr%levels(l)%boxes(b)%dims(1)
              val = l * 1d1 + b * 1d0 + i * 1d-1 + j * 1d-2 + k * 1d-3
              do uid = hyid%rho, hyid%e_tot
                samr%levels(l)%boxes(b)%hydro(i,j,k)%u = val + uid * 1d-4
              end do
            end do
          end do
        end do
      end do
    end do


    ! Initializing samr1d
    samr1d%nlevels = nlevels
    samr1d%max_nboxes(:) = max_nboxes(:)
    samr1d%ghost_cells(:) = ghost_cells_1d(:)
    samr1d%base_grid(:) = base_grid_1d(:)

    allocate ( samr1d%levels(0)%boxes(1)%hydro (
      lb(1):ub(1), lb(2):ub(2), lb(3):ub(3)
    ))

    do l = 0, 0 ! Initializing only the first level
      do b = 1, samr%levels(l)%nboxes
        do k = 1, samr%levels(l)%boxes(b)%dims(3)
          do j = 1, samr%levels(l)%boxes(b)%dims(2)
            do i = 1, samr%levels(l)%boxes(b)%dims(1)
              val = l * 1d1 + b * 1d0 + i * 1d-1 + j * 1d-2 + k * 1d-3
              do uid = hyid%rho, hyid%e_tot
                samr%levels(l)%boxes(b)%hydro(i,j,k)%u = val + uid * 1d-4
              end do
            end do
          end do
        end do
      end do
    end do
  end subroutine rhyme_samr_factory_init
end module rhyme_samr_factory
