module rhyme_samr_factory
  use rhyme_samr
  use rhyme_hydro_base

  implicit none

contains
  subroutine rhyme_samr_factory_fill ( &
    nlevels, base_grid, ghost_cells, max_nboxes, init_nboxes, samr, physical )
    implicit none

    integer, intent ( in ) :: nlevels, base_grid(3), ghost_cells(3)
    integer, intent ( in ) :: max_nboxes( 0:samrid%max_nlevels )
    integer, intent ( in ) :: init_nboxes( 0:samrid%max_nlevels )
    type ( samr_t ), intent ( out ) :: samr
    logical, intent ( in ), optional :: physical

    integer :: l, b, i, j, k, uid, lb(3), ub(3), box_dims(3), rand_len
    real ( kind=8 ) :: val
    logical :: phys = .false.
    type ( hydro_conserved_t ) :: state

    if ( present( physical ) ) then
      phys = physical
      rand_len = 5
      call random_seed( size = rand_len )
    else
      phys = .false.
    end if

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

      samr%levels(l)%iteration = 0
      samr%levels(l)%dx = 1.d0 / samr%base_grid / 2.d0**l
      samr%levels(l)%dt = 0.d0
      samr%levels(l)%t = 0.d0

      box_dims = floor( base_grid / real( init_nboxes(l) ) )
      box_dims = merge( box_dims, 1, box_dims > 1 )

      do b = 1, samr%levels(l)%nboxes
        samr%levels(l)%boxes(b)%level = l
        samr%levels(l)%boxes(b)%number = b

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
              if ( phys ) then
                state = gen_state()
                samr%levels(l)%boxes(b)%hydro(i,j,k) = state
              else
                val = l * 1d1 + b * 1d0 + i * 1d-2 + j * 1d-4 + k * 1d-6
                samr%levels(l)%boxes(b)%flags(i,j,k) = int ( val * 1e3 )

                do uid = hyid%rho, hyid%e_tot
                  samr%levels(l)%boxes(b)%hydro(i,j,k)%u(uid) = val + uid * 1d-7
                end do
              end if
            end do
          end do
        end do

      end do
    end do

    samr%initialized = .true.

  contains
    type ( hydro_conserved_t ) function gen_state () result ( U )
      implicit none

      real ( kind=8 ) :: r(5)

      call random_number( r )

      U%u( hyid%rho ) = r(1)
      U%u( hyid%rho_u ) = r(1) * ( r(2) - .5d0 )
      U%u( hyid%rho_v ) = r(1) * ( r(3) - .5d0 )
      U%u( hyid%rho_w ) = r(1) * ( r(4) - .5d0 )
      U%u( hyid%e_tot ) = .5d0 * sum( U%u( hyid%rho_u:hyid%rho_w )**2 ) / r(1) &
        + r(5) / ( 5.d0 / 3.d0 - 1 )
    end function gen_state
  end subroutine rhyme_samr_factory_fill
end module rhyme_samr_factory
