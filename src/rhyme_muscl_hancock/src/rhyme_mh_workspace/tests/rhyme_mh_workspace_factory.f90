module rhyme_mh_workspace_factory
  use rhyme_mh_workspace
  use rhyme_samr

  implicit none

  logical :: mh_ws_initialized = .false.

  integer, parameter :: nlevels = 4
  integer, parameter :: base_grid(3) = [ 16, 8 , 1 ]
  integer, parameter :: ghost_cells(3) = [ 2, 1, 0 ]

  integer, parameter :: max_nboxes ( 0:samrid%max_nlevels ) = [ &
    1, 3, 9, 27, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 &
  ]
  integer, parameter :: mh_init_nboxes ( 0:samrid%max_nlevels ) = [ &
    1, 2, 4, 8, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 &
  ]

  type(samr_t) :: samr

contains

  subroutine rhyme_mh_workspace_factory_init ()
    implicit none

    integer :: l, b, i, j, k, uid, lb(3), ub(3), dims(3)
    real ( kind=8 ) :: val


    if ( mh_ws_initialized ) return


    samr%nlevels = nlevels
    samr%base_grid = base_grid
    samr%max_nboxes = max_nboxes
    samr%ghost_cells = ghost_cells

    do l = 0, samr%nlevels
      samr%levels(l)%nboxes = mh_init_nboxes(l)
      allocate ( samr%levels(l)%boxes( samr%levels(l)%nboxes ) )

      do b = 1, samr%levels(l)%nboxes
        lb = - ghost_cells + 1
        dims = base_grid + l * 10 + (b - 1) * 1
        ub = dims + ghost_cells

        samr%levels(l)%boxes(b)%dims = dims
        samr%levels(l)%max_nboxes = max_nboxes(l)

        allocate ( samr%levels(l)%boxes(b)%flags( &
          lb(1):ub(1), lb(2):ub(2), lb(3):ub(3) &
        ) )

        allocate ( samr%levels(l)%boxes(b)%hydro( &
          lb(1):ub(1), lb(2):ub(2), lb(3):ub(3) &
        ) )

        do k = 1, samr%levels(l)%boxes(b)%dims(3)
          do j = 1, samr%levels(l)%boxes(b)%dims(2)
            do i = 1, samr%levels(l)%boxes(b)%dims(1)
              val = l * 1d1 + b * 1d0 + i * 1d-1 + j * 1d-2 + k * 1d-3
              samr%levels(l)%boxes(b)%flags(i,j,k) = int ( val * 1e3 )

              do uid = hyid%rho, hyid%e_tot
                samr%levels(l)%boxes(b)%hydro(i,j,k)%u = val + uid * 1d-4
              end do

            end do
          end do
        end do

      end do
    end do

    mh_ws_initialized = .true.

  end subroutine rhyme_mh_workspace_factory_init
end module rhyme_mh_workspace_factory
