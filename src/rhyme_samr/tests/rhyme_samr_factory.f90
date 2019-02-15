module rhyme_samr_factory
  use rhyme_samr

  implicit none

  integer, parameter :: nlevels = 4
  integer, parameter :: base_grid(3) = [ 16, 8, 4 ]
  integer, parameter :: ghost_cells(3) = [ 2, 1, 0 ]

  integer, parameter :: base_grid_1d(3) = [ 32, 1, 1 ]
  integer, parameter :: ghost_cells_1d(3) = [ 2, 0, 0 ]

  integer, parameter :: base_grid_uni(3) = [ 32, 1, 1 ]
  integer, parameter :: ghost_cells_uni(3) = [ 2, 0, 0 ]

  integer, parameter :: max_nboxes ( 0:samrid%max_nlevels ) = [ &
    1, 3, 9, 27, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 &
  ]
  integer, parameter :: max_nboxes_uni ( 0:samrid%max_nlevels ) = [ &
    1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 &
  ]
  integer, parameter :: nboxes ( 0:samrid%max_nlevels ) = [ &
    1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 &
  ]

  type(samr_t) :: &
  samr = samr_t ( .false., nlevels, base_grid, ghost_cells, max_nboxes ), &
  samr1d = samr_t ( .false., nlevels, base_grid_1d, ghost_cells_1d, max_nboxes ), &
  samr_uni = samr_t ( .false., 1, base_grid_uni, ghost_cells_uni, max_nboxes_uni )


contains

  subroutine rhyme_samr_factory_init ()
    implicit none

    integer :: l, b, i, j, k, uid
    integer, dimension(3) :: lb, ub, lb1d, ub1d, lbuni, ubuni
    real ( kind=8 ) :: val


    ! Initializing samr
    lb = - ghost_cells + 1
    ub = base_grid + ghost_cells

    allocate ( samr%levels(0)%boxes( samr%levels(0)%max_nboxes ) )
    allocate ( samr%levels(0)%boxes(1)%hydro ( &
      lb(1):ub(1), lb(2):ub(2), lb(3):ub(3) &
    ))

    do l = 0, samr%nlevels - 1
      if ( l .ne. 0 ) allocate ( samr%levels(l)%boxes( samr%levels(l)%nboxes ) )
      do b = 1, samr%levels(l)%nboxes
        do k = 1, samr%levels(l)%boxes(b)%dims(3)
          do j = 1, samr%levels(l)%boxes(b)%dims(2)
            do i = 1, samr%levels(l)%boxes(b)%dims(1)
              val = l * 1d1 + b * 1d0 + i * 1d-2 + j * 1d-4 + k * 1d-6
              do uid = hyid%rho, hyid%e_tot
                samr%levels(l)%boxes(b)%hydro(i,j,k)%u(uid) = val + uid * 1d-7
              end do
            end do
          end do
        end do
      end do
    end do


    ! Initializing samr1d
    lb1d = - ghost_cells_1d
    ub1d = base_grid_1d + ghost_cells_1d

    allocate ( samr1d%levels(0)%boxes( samr1d%levels(0)%max_nboxes ) )
    allocate ( samr1d%levels(0)%boxes(1)%hydro ( &
      lb1d(1):ub1d(1), lb1d(2):ub1d(2), lb1d(3):ub1d(3) &
    ))

    do l = 0, 0 ! Initializing only the first level
      do b = 1, samr1d%levels(l)%nboxes
        do k = 1, samr1d%levels(l)%boxes(b)%dims(3)
          do j = 1, samr1d%levels(l)%boxes(b)%dims(2)
            do i = 1, samr1d%levels(l)%boxes(b)%dims(1)
              val = l * 1d1 + b * 1d0 + i * 1d-1 + j * 1d-2 + k * 1d-3
              do uid = hyid%rho, hyid%e_tot
                samr1d%levels(l)%boxes(b)%hydro(i,j,k)%u = val + uid * 1d-4
              end do
            end do
          end do
        end do
      end do
    end do


    ! Initializing samr_uni
    lbuni = - ghost_cells_uni
    ubuni = base_grid_uni + ghost_cells_uni

    allocate ( samr_uni%levels(0)%boxes( samr_uni%levels(0)%max_nboxes ) )
    allocate ( samr_uni%levels(0)%boxes(1)%hydro ( &
      lbuni(1):ubuni(1), lbuni(2):ubuni(2), lbuni(3):ub1d(3) &
    ))

    do l = 0, 0 ! Initializing only the first level
      do b = 1, samr_uni%levels(l)%nboxes
        do k = 1, samr_uni%levels(l)%boxes(b)%dims(3)
          do j = 1, samr_uni%levels(l)%boxes(b)%dims(2)
            do i = 1, samr_uni%levels(l)%boxes(b)%dims(1)
              val = l * 1d1 + b * 1d0 + i * 1d-1 + j * 1d-2 + k * 1d-3
              do uid = hyid%rho, hyid%e_tot
                samr_uni%levels(l)%boxes(b)%hydro(i,j,k)%u = val + uid * 1d-4
              end do
            end do
          end do
        end do
      end do
    end do
  end subroutine rhyme_samr_factory_init
end module rhyme_samr_factory
