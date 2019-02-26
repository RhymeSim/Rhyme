module rhyme_chombo_factory
  use rhyme_chombo
  use rhyme_samr


  implicit none


  integer, parameter :: base_grid(3) = [ 16, 8, 1 ]
  integer, parameter :: nlevels = 3
  integer, parameter :: chombo_factory_ghost_cells(3) = [ 2, 1, 1 ]

  integer, parameter :: chombo_factory_max_nboxes ( 0:samrid%max_nlevels ) = [ &
    1, 10, 100, 1000, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 &
  ]
  integer, parameter :: chombo_factory_init_nboxes ( 0:samrid%max_nlevels ) = [ &
    1, 2, 4, 8, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 &
  ]

  type(samr_t) :: samr

contains

  subroutine rhyme_chombo_factory_init ()
    implicit none

    integer :: l, b, i, j, k, uid, lb(3), ub(3), dims(3)
    real ( kind=8 ) :: val

    samr%base_grid = base_grid
    samr%nlevels = nlevels
    samr%max_nboxes = chombo_factory_max_nboxes
    samr%ghost_cells = chombo_factory_ghost_cells

    do l = 0, samr%nlevels - 1
      if ( .not. allocated( samr%levels(l)%boxes ) ) then
        allocate( samr%levels(l)%boxes( samr%max_nboxes(l) ) )
      end if

      do b = 1, chombo_factory_init_nboxes(l)

        if ( .not. allocated( samr%levels(l)%boxes(b)%hydro ) ) then
          lb = - chombo_factory_ghost_cells + 1
          dims = base_grid + l * 10 + (b - 1)
          ub = dims + chombo_factory_ghost_cells

          allocate ( samr%levels(l)%boxes(b)%hydro( &
            lb(1):ub(1),lb(2):ub(2),lb(3):ub(3) &
          ) )

          samr%levels(l)%boxes(b)%dims = dims
          samr%levels(l)%nboxes = samr%levels(l)%nboxes + 1

          samr%levels(l)%boxes(b)%left_edge = [ 1, 2, 3 ]
          samr%levels(l)%boxes(b)%right_edge = [ 4, 5, 6 ]
        end if

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
  end subroutine rhyme_chombo_factory_init

end module rhyme_chombo_factory
