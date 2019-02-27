module rhyme_samr_factory
  use rhyme_samr

  implicit none

contains
  subroutine rhyme_samr_factory_fill ( &
    nlevels, base_grid, ghost_cells, max_nboxes, init_nboxes, samr )
    implicit none

    integer, intent ( in ) :: nlevels, base_grid(3), ghost_cells(3)
    integer, intent ( in ) :: max_nboxes( 0:samrid%max_nlevels )
    integer, intent ( in ) :: init_nboxes( 0:samrid%max_nlevels )
    type ( samr_t ), intent ( out ) :: samr

    integer :: l, b, i, j, k, uid, lb(3), ub(3), box_dims(3)
    real ( kind=8 ) :: val

    if ( samr%initialized ) then
      print *, 'SAMR has already been initialized'
      return
    end if

    samr%nlevels = nlevels
    samr%base_grid = base_grid
    samr%ghost_cells = ghost_cells
    samr%max_nboxes = max_nboxes

    samr%levels%level = [ ( l, l=0, 23 ) ]
    samr%levels%max_nboxes = max_nboxes
    samr%levels%nboxes = init_nboxes

    do l = 0, samr%nlevels - 1
      allocate ( samr%levels(l)%boxes( samr%levels(l)%max_nboxes ) )

      box_dims = floor( base_grid / real( init_nboxes(l) ) )
      box_dims = merge( box_dims, 1, box_dims > 0 )

      do b = 1, samr%levels(l)%nboxes
        lb = -ghost_cells + 1
        ub = box_dims + ghost_cells

        samr%levels(l)%boxes(b)%dims = box_dims

        allocate ( samr%levels(l)%boxes(b)%hydro(lb(1):ub(1), lb(2):ub(2), lb(3):ub(3)) )
        allocate ( samr%levels(l)%boxes(b)%flags(lb(1):ub(1), lb(2):ub(2), lb(3):ub(3)) )

        samr%levels(l)%boxes(b)%left_edge = (b - 1) * box_dims + 1
        samr%levels(l)%boxes(b)%right_edge = b * box_dims

        do k = 1, samr%levels(l)%boxes(b)%dims(3)
          do j = 1, samr%levels(l)%boxes(b)%dims(2)
            do i = 1, samr%levels(l)%boxes(b)%dims(1)
              val = l * 1d1 + b * 1d0 + i * 1d-2 + j * 1d-4 + k * 1d-6
              samr%levels(l)%boxes(b)%flags(i,j,k) = int ( val * 1e3 )

              do uid = hyid%rho, hyid%e_tot
                samr%levels(l)%boxes(b)%hydro(i,j,k)%u(uid) = val + uid * 1d-7
              end do
            end do
          end do
        end do

      end do
    end do

    samr%initialized = .true.
  end subroutine rhyme_samr_factory_fill
end module rhyme_samr_factory
