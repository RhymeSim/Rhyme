submodule ( rhyme_samr_bc ) init_smod
contains
  module subroutine rhyme_samr_bc_init ( bc, samr, logger )
    ! NB: Only boxes residing at level 0 are being handeled here. If the ghost
    !     region of higher level boxes overlap the boundaries, their values
    !     will be set based on level 0 boxes later on
    implicit none

    type ( samr_bc_t ), intent ( inout ) :: bc
    type ( samr_t ), intent ( inout ) :: samr
    type ( logger_t ), intent ( inout ) :: logger

#if NDIM == 1
#define IFJ
#define IFK
#define JDX
#define KDX
#define LOOP_J
#define LOOP_J_END
#define LOOP_K
#define LOOP_K_END
#endif

#if NDIM == 2
#define IFJ .or. j + di(2) < 1 .or. j + di(2) > samr%levels(l)%boxes(b)%dims(2) &
#define IFK
#define JDX ,j
#define KDX
#define LOOP_J do j = lb(2), ub(2)
#define LOOP_J_END end do
#define LOOP_K
#define LOOP_K_END
#endif

#if NDIM == 3
#define IFJ .or. j + di(2) < 1 .or. j + di(2) > samr%levels(l)%boxes(b)%dims(2) &
#define IFK .or. k + di(3) < 1 .or. k + di(3) > samr%levels(l)%boxes(b)%dims(3)
#define JDX ,j
#define KDX ,k
#define LOOP_J do j = lb(2), ub(2)
#define LOOP_J_END end do
#define LOOP_K do k = lb(3), ub(3)
#define LOOP_K_END end do
#endif

    integer :: l, b, i JDX KDX, uid, lb( NDIM ), ub( NDIM ), di( NDIM )

    call logger%begin_section( 'samr_bc' )

    do l = 0, 0 ! Only level 0 boxes are responsible for boundaries
      do b = 1, samr%levels(l)%nboxes

        lb = lbound( samr%levels(l)%boxes(b)%flags )
        ub = ubound( samr%levels(l)%boxes(b)%flags )

        di = samr%levels(l)%boxes(b)%left_edge - 1

        do uid = cid%rho, cid%e_tot
          LOOP_K
            LOOP_J

              do i = lb(1), ub(1)
                if ( i + di(1) < 1 .or. i + di(1) > samr%levels(l)%boxes(b)%dims(1) &
                IFJ
                IFK ) then
                  samr%levels(l)%boxes(b)%flags( i JDX KDX ) = samrid%ghost
                  samr%levels(l)%boxes(b)%cells( i JDX KDX, uid ) = 0.d0
                end if
              end do

            LOOP_J_END
          LOOP_K_END
        end do

      end do
    end do

    call logger%end_section
  end subroutine rhyme_samr_bc_init
end submodule init_smod
