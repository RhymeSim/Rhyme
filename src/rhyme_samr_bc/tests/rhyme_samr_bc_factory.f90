module rhyme_samr_bc_factory
  use rhyme_samr
  use rhyme_samr_bc

  implicit none

  logical :: initialized = .false.
  integer, parameter :: base_grid(3) = [ 16, 8, 4 ]
  integer, parameter :: nlevels = 3
  integer, parameter :: nboxes = 11
  integer, parameter :: ghost_cells(3) = [ 2, 2, 2 ]
  integer, parameter :: tot_nboxes(0:23) = [ &
    1, 10, 100, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 &
  ]

  integer :: bc_types(6) = [ &
    bcid%reflective, &
    bcid%outflow, &
    bcid%periodic, &
    bcid%reflective, &
    bcid%outflow, &
    bcid%periodic ]

  type(samr_t) :: samr

contains

  subroutine rhyme_samr_bc_factory_init ()
    implicit none

    integer :: l, b, i, j, k, uid
    real ( kind=8 ) :: val

    if ( initialized ) return

    call samr%init_with ( base_grid, nlevels, tot_nboxes, ghost_cells )

    do l = 0, samr%nlevels - 1
      do b = 1, samr%levels(l)%nboxes
        do k = 1, samr%base_grid(3)
          do j = 1, samr%base_grid(2)
            do i = 1, samr%base_grid(1)
              val = l * 1d1 + b * 1d0 + i * 1d-2 + j * 1d-4 + k * 1d-6
              do uid = hyid%rho, hyid%e_tot
                samr%levels(l)%boxes(b)%hydro(i,j,k)%u(uid) = val + uid * 1d-7
              end do
            end do
          end do
        end do
      end do
    end do
  end subroutine rhyme_samr_bc_factory_init

end module rhyme_samr_bc_factory
