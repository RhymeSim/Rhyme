submodule ( rhyme_drawing ) rhyme_drawing_uniform_cuboid_submoduel
contains
  module subroutine rhyme_drawing_uniform_cuboid ( samr, shape )
    implicit none

    type ( samr_t ), intent ( inout ) :: samr
    type ( shape_t ), intent ( in ) :: shape

#if NDIM == 1
#define JDX
#define KDX
#define LOOP_J
#define LOOP_K
#define LOOP_J_END
#define LOOP_K_END
#elif NDIM == 2
#define JDX ,j
#define KDX
#define LOOP_J do j = 1, samr%levels(l)%boxes(b)%dims(2)
#define LOOP_K
#define LOOP_J_END end do
#define LOOP_K_END
#elif NDIM == 3
#define JDX ,j
#define KDX ,k
#define LOOP_J do j = 1, samr%levels(l)%boxes(b)%dims(2)
#define LOOP_K do k = 1, samr%levels(l)%boxes(b)%dims(3)
#define LOOP_J_END end do
#define LOOP_K_END end do
#endif

    integer :: l, b, i JDX KDX, uid
    integer :: shift( NDIM ), lb( NDIM ), ub( NDIM )
    real ( kind=8 ) :: color( cid%rho:cid%e_tot )

    call conv_prim_to_cons( shape%fill%colors( cid%rho:cid%p, 1 ), color )

    do l = 0, samr%nlevels - 1
      do b = 1, samr%levels(l)%nboxes
        shift = samr%levels(l)%boxes(b)%left_edge

        lb = shape%cuboid%left_corner * 2**l - shift
        lb = merge( 1, lb, lb < 1 )
        lb = merge( samr%levels(l)%boxes(b)%dims + 1, lb, lb > samr%levels(l)%boxes(b)%dims )

        ub = ( shape%cuboid%left_corner + shape%cuboid%lengths ) * 2**l - shift
        ub = merge( 0, ub, ub < 1 )
        ub = merge( samr%levels(l)%boxes(b)%dims, ub, ub > samr%levels(l)%boxes(b)%dims )


        do uid = cid%rho, cid%e_tot
          LOOP_K
            LOOP_J
              do i = lb(1), ub(1)
                samr%levels(l)%boxes(b)%cells( i JDX KDX, uid ) = color( uid )
              end do
            LOOP_J_END
          LOOP_K_END
        end do

      end do
    end do
  end subroutine rhyme_drawing_uniform_cuboid
end submodule rhyme_drawing_uniform_cuboid_submoduel
