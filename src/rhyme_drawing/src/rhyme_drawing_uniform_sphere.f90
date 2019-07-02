submodule ( rhyme_drawing ) rhyme_drawing_uniform_sphere_submodule
contains
  module subroutine rhyme_drawing_uniform_sphere ( samr, shape )
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
#define LOOP_J do j = lb(2), ub(2)
#define LOOP_K
#define LOOP_J_END end do
#define LOOP_K_END
#elif NDIM == 3
#define JDX ,j
#define KDX ,k
#define LOOP_J do j = lb(2), ub(2)
#define LOOP_K do k = lb(3), ub(3)
#define LOOP_J_END end do
#define LOOP_K_END end do
#endif

    real ( kind=8 ) :: color( cid%rho:cid%e_tot )
    integer :: l, b, i JDX KDX
    integer :: shift( NDIM ), lb( NDIM ), ub( NDIM )

    call conv_prim_to_cons( shape%fill%colors( cid%rho:cid%p, 1 ), color )

    do l = 0, samr%nlevels - 1
      do b = 1, samr%levels(l)%nboxes
        shift = samr%levels(l)%boxes(b)%left_edge

        lb = ( int( shape%sphere%origin - shape%sphere%r ) - 1 ) * 2**l - shift
        lb = merge( 1, lb, lb < 1 )
        lb = merge( samr%levels(l)%boxes(b)%dims + 1, lb, lb > samr%levels(l)%boxes(b)%dims )

        ub = ( int( shape%sphere%origin + shape%sphere%r ) + 1 ) * 2**l - shift
        ub = merge( 0, ub, ub < 1 )
        ub = merge( samr%levels(l)%boxes(b)%dims, ub, ub > samr%levels(l)%boxes(b)%dims )

        LOOP_K
          LOOP_J
            do i = lb(1), ub(1)
              if ( is_inside_sphere( [ i JDX KDX ], samr%levels(l)%boxes(b), shape ) ) then
                samr%levels(l)%boxes(b)%cells( i JDX KDX, cid%rho:cid%e_tot ) = color
              end if
            end do
          LOOP_J_END
        LOOP_K_END
      end do
    end do

  contains
    pure logical function is_inside_sphere ( point, box, shape ) result ( is_inside )
      implicit none

      integer, intent ( in ) :: point( NDIM )
      type ( samr_box_t ), intent ( in ) :: box
      type ( shape_t ), intent ( in ) :: shape

      real ( kind=8 ) :: p0( NDIM ), r2

      p0 = real( point, kind=8 ) / 2**box%level
      r2 = shape%sphere%r**2

      if ( sum( (p0 - shape%sphere%origin)**2 ) > r2 ) then
        is_inside = .false.
      else
        is_inside = .true.
      end if
    end function is_inside_sphere
  end subroutine rhyme_drawing_uniform_sphere
end submodule rhyme_drawing_uniform_sphere_submodule
